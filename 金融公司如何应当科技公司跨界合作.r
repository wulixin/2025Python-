
# 加载包
library(DALEX)
library(ggplot2)
library(dplyr)

set.seed(42)
n <- 1000

# 生成基础数据
data <- data.frame(
  loan_id = 1:n,
  age_years = runif(n, 1, 20),
  annual_revenue = exp(rnorm(n, mean = 10, sd = 0.8)) / 1e4 * 1000,  # 百万级营收
  profit_margin = pmax(pmin(rnorm(n, 8, 5), 25), 1),                 # 净利润率 1~25%
  debt_ratio = rbeta(n, 2, 3) * 100,                                  # 资产负债率
  credit_score_bank = pmax(pmin(round(rnorm(n, 650, 100)), 850), 300), # 银行信用分
  use_b_platform = sample(c(TRUE, FALSE), n, prob = c(0.6, 0.4), replace = TRUE) # 是否使用B平台
)

# 根据 use_b_platform 决定交易额和履约率
data$transaction_volume_b <- ifelse(
  data$use_b_platform,
  rexp(n, 1/50),   # 平均50万的指数分布
  0
)

data$on_time_delivery_rate <- ifelse(
  data$use_b_platform,
  rbeta(n, 8, 1.5) * 100,  # 高准时率
  NA                    # 不使用者设为缺失
)

# 其他字段
data$employee_count <- sample(c(10, 20, 50, 100), n, replace = TRUE,
                              prob = c(0.4, 0.3, 0.2, 0.1))
data$has_physical_factory <- sample(c(1, 0), n, replace = TRUE, prob = c(0.7, 0.3))
data$region_risk_level <- sample(1:3, n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
data$loan_amount_requested <- runif(n, 50, 500)

# 填补 on_time_delivery_rate 缺失值（用中位数）
median_delivery <- median(data$on_time_delivery_rate, na.rm = TRUE)
data$on_time_delivery_rate[is.na(data$on_time_delivery_rate)] <- median_delivery

# 删除临时列
data$use_b_platform <- NULL

# 查看前几行
head(data)


# 定义特征变量
features <- c(
  "age_years",
  "annual_revenue",
  "profit_margin",
  "debt_ratio",
  "credit_score_bank",
  "transaction_volume_b",
  "on_time_delivery_rate",
  "employee_count",
  "has_physical_factory",
  "region_risk_level",
  "loan_amount_requested"
)


# 构造审批目标变量（基于综合评分）
X_score <- (
  0.02 * data$age_years +
    0.0001 * data$annual_revenue +
    0.1 * data$profit_margin -
    0.03 * data$debt_ratio +
    0.005 * data$credit_score_bank +
    0.00005 * data$transaction_volume_b +
    0.01 * data$on_time_delivery_rate +
    0.5 * data$has_physical_factory -
    0.3 * (data$region_risk_level - 1) -
    0.001 * data$loan_amount_requested
)


# Sigmoid 转换为概率
prob <- 1 / (1 + exp(-(X_score - mean(X_score))))
data$approved <- as.factor(ifelse(runif(n) < prob, "Yes", "No"))

# 训练集测试集划分
idx_train <- sample(1:n, size = 0.8 * n)
train_data <- data[idx_train, ]
test_data  <- data[-idx_train, ]
# 确保 approved 是因子，且只保留需要的列
train_data_model <- train_data %>%
  select(all_of(features), approved)

# 建模（不需要手动子集传入 data）
model_glm <- glm(approved ~ ., 
                 data = train_data_model, 
                 family = binomial)

# 查看结果
summary(model_glm)

# 创建 explainer 对象
explainer_glm <- explain(model_glm,
                         data = select(test_data, all_of(features)),
                         y = test_data$approved == "Yes",
                         label = "Logistic Regression Model")

# 显示基本信息
print(explainer_glm)

vi <- model_parts(explainer_glm)
head(vi)

# 绘图
plot(vi, max_vars = 10) +
  ggtitle("Variable Importance (Permutation)")

# 选择一个测试样本（例如第一个）
new_observation <- test_data[1, features]

bd <- predict_parts(explainer_glm,
                    new_observation,
                    type = "break_down")

plot(bd) +
  ggtitle("Break-down: Why This Loan Was Approved?")



shap <- predict_parts(explainer_glm,
                      new_observation,
                      type = "shap",
                      B = 25)  # 设置采样次数

plot(shap) +
  ggtitle("SHAP Values for Individual Prediction")


# 信用评分的影响
pd_credit <- model_profile(explainer_glm, variables = "credit_score_bank")
plot(pd_credit) + ggtitle("Partial Dependence: Credit Score vs Approval Probability")

# B平台交易额的影响（体现合作价值）
pd_trans <- model_profile(explainer_glm, variables = "transaction_volume_b")
plot(pd_trans) + ggtitle("Impact of B-Platform Transaction Volume")


ice_trans <- model_profile(explainer_glm, type = "partial", variables = "transaction_volume_b")
plot(ice_trans) + ggtitle("Individual Conditional Expectation (ICE)")


mp <- model_performance(explainer_glm)
plot(mp)  # ROC Curve, LIFT Chart

mp$measurements %>% filter(measure %in% c("AUROC"))

mp$measurements %>% filter(measure %in% c("AUROC"))

#############################################
#
#     加入B平台数据与不加入B平台数据做对比
#
##############################################
# 确保 features 已正确定义
features_full <- c(
  "age_years", "annual_revenue", "profit_margin", "debt_ratio",
  "credit_score_bank", "transaction_volume_b", "on_time_delivery_rate",
  "employee_count", "has_physical_factory", "region_risk_level",
  "loan_amount_requested"
)

# 去掉B平台相关字段 → 模拟A银行独立建模能力
features_base <- setdiff(features_full, c("transaction_volume_b", "on_time_delivery_rate"))


library(dplyr)

# 融合模型数据（含B平台）
train_full <- train_data %>%
  select(all_of(features_full), approved)

test_full <- test_data %>%
  select(all_of(features_full), approved)

# 基础模型数据（不含B平台）
train_base <- train_data %>%
  select(all_of(features_base), approved)

test_base <- test_data %>%
  select(all_of(features_base), approved)



# 模型1：融合模型（含B平台数据）
model_full <- glm(approved ~ ., data = train_full, family = binomial)

# 模型2：基础模型（无B平台数据）
model_base <- glm(approved ~ ., data = train_base, family = binomial)



library(DALEX)

# 解释器1：融合模型
# 解释器1：融合模型
explainer_full <- DALEX::explain(
  model_full,
  data = test_full[, features_full],  # 测试集X
  y = test_full$approved == "Yes",   # y为逻辑值TRUE/FALSE
  label = "Model with 京东 Platform Data"
)

# 解释器2：基础模型
explainer_base <- DALEX::explain(
  model = model_base,
  data = test_base[, features_base, drop = FALSE],
  y = test_base$approved == "Yes",
  label = "Model without 京东 Platform Data"
)

mp_full <- model_performance(explainer_full)
mp_base <- model_performance(explainer_base)

# 对比绘图
plot(mp_full, mp_base) +
  ggtitle("ROC Curve: With vs Without 京东 Platform Data") +
  scale_color_brewer(type = "qual", palette = "Set1")



plot(mp_full, mp_base, geom = "lift") +
  ggtitle("Lift Chart: Model Comparison")



vi_full <- model_parts(explainer_full)
vi_base <- model_parts(explainer_base)

plot(vi_full, vi_base) +
  ggtitle("Variable Importance: Impact of Adding 京东 Platform Data")


# 提取第一个测试样本
new_obs_full <- test_full[1, features_full, drop = FALSE]
new_obs_base <- test_full[1, features_base, drop = FALSE]  # 注意：从 test_full 中提取并去除非base列

# 查看该客户信息
print("Customer Info:")
print(t(new_obs_full))


# Full 模型解释
bd_full <- predict_parts(explainer_full, new_obs_full, type = "break_down")
# Base 模型解释
bd_base <- predict_parts(explainer_base, new_obs_base, type = "break_down")

# 对比绘图
plot(bd_full, bd_base) +
  ggtitle("Break-down: With vs Without 京东 Platform Data") +
  theme(legend.position = "bottom")


# SHAP 解释（推荐用于生产环境）
shap_full <- predict_parts(explainer_full, new_obs_full, type = "shap", B = 25)
shap_base <- predict_parts(explainer_base, new_obs_base, type = "shap", B = 25)

# 绘图对比
plot(shap_full, shap_base) +
  ggtitle("SHAP Values: Impact of 京东 Platform Features") +
  coord_flip()  # 横向显示更清晰


# 分批分析多个客户
# 对前5个客户做 SHAP 分析
clients_full <- test_full[1:5, features_full, drop = FALSE]
clients_base <- test_full[1:5, features_base, drop = FALSE]

shap_multi_full <- predict_parts(explainer_full, clients_full, type = "shap", B = 10)
shap_multi_base <- predict_parts(explainer_base, clients_base, type = "shap", B = 10)

plot(shap_multi_full, max_vars = 8) + ggtitle("Top Features Driving Decisions (With 京东 Data)")
plot(shap_multi_base, max_vars = 8) + ggtitle("Only Traditional Features Matter (No 京东 Data)")



#使用 ROCR 包（或现代替代方案）计算 全面的性能指标表（Table 1 风格），包括： 

#分类准确性、敏感性、特异性
#精确率、召回率、F1
#提升度（Lift）、预测误差
#AUC、RMSE、交叉熵等


#从而揭示：即使 AUC 差异小，其他关键业务指标（如 precision, recall, lift）仍可能有显著改进。

# 加载
library(ROCR)

# 预测概率（正类概率）
prob_full <- predict(model_full, test_full, type = "response")
prob_base <- predict(model_base, test_base, type = "response")

# 真实标签
true_labels <- test_full$approved  # 因为 test_full 和 test_base 来自同一子集


pred_full <- prediction(prob_full, true_labels)
pred_base <- prediction(prob_base, true_labels)

library(ROCR)

compute_metrics <- function(pred_obj, label) {
  # 获取在阈值 0.5 附近的指标
  get_at_thres <- function(p, t = 0.5) {
    x_vals <- slot(p, 'x.values')[[1]]   # 阈值 or rpp
    y_vals <- slot(p, 'y.values')[[1]]
    idx <- which.min(abs(x_vals - t))
    return(y_vals[idx])
  }
  
  # 计算各项 performance 对象
  acc   <- performance(pred_obj, "acc")
  sens  <- performance(pred_obj, "sens")   # sensitivity = recall
  spec  <- performance(pred_obj, "spec")   # specificity
  prec  <- performance(pred_obj, "prec")   # precision
  fpr   <- performance(pred_obj, "fpr")    # false positive rate = fallout
  fnr   <- performance(pred_obj, "fnr")    # miss rate
  npv   <- performance(pred_obj, "npv")    # negative predictive value
  
  list(
    Model = label,
    Accuracy       = get_at_thres(acc, 0.5),
    Sensitivity    = get_at_thres(sens, 0.5),
    Specificity    = get_at_thres(spec, 0.5),
    Precision      = get_at_thres(prec, 0.5),
    Recall         = get_at_thres(sens, 0.5),  # same as sensitivity
    NPV            = get_at_thres(npv, 0.5),
    Fallout        = get_at_thres(fpr, 0.5),  # 即 FPR
    Miss_Rate      = get_at_thres(fnr, 0.5)
  )
}



# 计算两个模型的指标
metrics_full <- compute_metrics(pred_full, "With B Platform")
metrics_base <- compute_metrics(pred_base, "Without B Platform")

# 合并为表格
perf_table <- rbind(as.data.frame(metrics_full), as.data.frame(metrics_base))
perf_table$F1_Score <- 2 * (perf_table$Precision * perf_table$Recall) / 
  (perf_table$Precision + perf_table$Recall)

# 打印结果
print(perf_table, digits = 3)


auc_full <- performance(pred_full, "auc")@y.values[[1]]
auc_base <- performance(pred_base, "auc")@y.values[[1]]
cat("AUC (Full):", round(auc_full, 3), "\n")
cat("AUC (Base):", round(auc_base, 3), "\n")


lift_full <- performance(pred_full, "lift", "rpp")  # rpp = rate of positive predictions
plot(lift_full, main = "Lift Curve: With vs Without B Platform")

# 获取 top 10% 样本时的 lift 值
rpp_vals <- slot(lift_full, 'x.values')[[1]]
lift_vals <- slot(lift_full, 'y.values')[[1]]
lift_at_10 <- approx(rpp_vals, lift_vals,xout = 10)$y

rmse <- function(labels, probs) {
  sqrt(mean((as.numeric(labels) - 1) - probs)^2)
}
rmse_full <- rmse(true_labels, prob_full)
rmse_base <- rmse(true_labels, prob_base)


logloss <- function(labels, probs) {
  eps <- 1e-15  # 防止 log(0)
  probs <- pmax(pmin(probs, 1 - eps), eps)
  -mean(as.numeric(labels == "Yes") * log(probs) + 
          (1 - as.numeric(labels == "Yes")) * log(1 - probs))
}

logloss_full <- logloss(true_labels, prob_full)
logloss_base <- logloss(true_labels, prob_base)

指标	业务含义	战略启示
↑ Recall / ↓ Miss Rate	更少漏掉好客户	可拓展服务边界
↑ Precision / ↑ F1	减少误批风险	控制不良率同时扩大规模
↑ Lift	营销资源更高效	推出“A+B白名单精准推送”
↓ Log Loss	概率更可靠	支持动态定价与额度管理


logloss <- function(labels, probs) {
  eps <- 1e-15
  probs <- pmax(pmin(probs, 1 - eps), eps)
  actual <- ifelse(labels == "Yes", 1, 0)
  -mean(actual * log(probs) + (1 - actual) * log(1 - probs))
}

ll_full <- logloss(true_labels, prob_full)
ll_base <- logloss(true_labels, prob_base)


get_lift_at <- function(pred_obj, p = 0.1) {
  # p: top p% of samples (e.g., 0.1 for top 10%)
  perf_lift <- performance(pred_obj, "lift", "rpp")
  rpp <- perf_lift@x.values[[1]]
  lift <- perf_lift@y.values[[1]]
  
  # 确保 rpp 是数值型，并排序
  df <- data.frame(rpp = rpp, lift = lift)
  df <- df[order(df$rpp), ]
  
  # 插值获取指定百分比下的 Lift
  approx(df$rpp, df$lift, xout = p)$y
}

# 使用示例
lift_at_10pct_full  <- get_lift_at(pred_full, 0.1)
lift_at_10pct_base <- get_lift_at(pred_base, 0.1)

cat("Lift @ Top 10% (Full):", round(lift_at_10pct_full, 2), "\n")
cat("Lift @ Top 10% (Base):", round(lift_at_10pct_base, 2), "\n")

final_table <- data.frame(
  Measure = c("Accuracy", "Sensitivity (Recall)", "Specificity", "Precision",
              "F1-Score", "NPV", "Fallout (FPR)", "Miss Rate (FNR)",
              "AUC", "Lift@ Top 10%", "Log Loss", "RMSE"),
  `With 京东 Platform` = c(
    round(metrics_full$Accuracy, 3),
    round(metrics_full$Sensitivity, 3),
    round(metrics_full$Specificity, 3),
    round(metrics_full$Precision, 3),
    round(perf_table$F1_Score[1], 3),
    round(metrics_full$NPV, 3),
    round(metrics_full$Fallout, 3),
    round(metrics_full$Miss_Rate, 3),
    round(auc_full, 3),
    round(lift_at_10pct_full, 1),
    round(ll_full, 3),
    round(rmse_full, 3)
  ),
  `Without 京东 Platform` = c(
    round(metrics_base$Accuracy, 3),
    round(metrics_base$Sensitivity, 3),
    round(metrics_base$Specificity, 3),
    round(metrics_base$Precision, 3),
    round(perf_table$F1_Score[2], 3),
    round(metrics_base$NPV, 3),
    round(metrics_base$Fallout, 3),
    round(metrics_base$Miss_Rate, 3),
    round(auc_base, 3),
    round(lift_at_10pct_base, 1),  # 类似计算
    round(ll_base, 3),
    round(rmse_base, 3)
  )
)

print(final_table)
library(DT)
datatable(final_table,options = list(
  pageLength = 15))
原因	解释
🔹 数据质量问题	B平台数据存在噪声或偏差（如虚假交易、刷单）
🔹 特征冗余/冲突	B平台行为与财务数据高度相关，引入后导致过拟合
🔹 样本偏差	使用B平台的企业本身信用偏高，导致模型“偏见”
🔹 模型未充分训练	融合数据后特征空间扩大，但样本不足，学习效果下降
🔹 缺乏正则化	多变量导致过拟合，泛化能力下降


# 哈哈居然推翻了之前的结论

客户特征	类型判定
有电商平台交易记录、高履约率	🟢 电商型客户
有厂房设备、供应链合同、出口报关单	🔵 制造业客户
混合模式（线上线下结合）	🟡 综合型客户



# 显示
print(final_table)


library(mlr3)
library(mlr3viz)
library(lime)
library(mlr3learners)
library(mlr3)
library(mlr3learners)

# --- 1. 创建训练任务 ---
task_full <- TaskClassif$new(
  id = "loan_full",
  backend = train_full,
  target = "approved"   # 必须指定目标列
)

# --- 2. 获取学习器 ---
learner_glm <- lrn("classif.log_reg", predict_type = "prob")

# --- 3. 训练模型 ---
model_mlr_full <- learner_glm$train(task_full)

# --- 4. 创建测试任务 ---
task_test_full <- TaskClassif$new(
  id = "test_full",
  backend = test_full,
  target = "approved"   # 同样需要 target！
)

# --- 5. 预测 ---
pred_full <- model_mlr_full$predict(task_test_full)

# --- 6. 查看结果 ---
print(pred_full)
pred_full$score(msr("classif.auc"))


pred_base <- model_mlr_base$predict(task_base)

# 定义逻辑回归 learner
learner_glm <- lrn("classif.log_reg", predict_type = "prob")

# 训练模型
model_mlr_full <- learner_glm$train(task_full)
model_mlr_base <- learner_glm$train(task_base)

# 预测测试集
pred_full <- model_mlr_full$predict(TaskClassif$new("test", backend = test_full))
pred_base <- model_mlr_base$predict(TaskClassif$new("test", backend = test_base))

# 性能对比
as.data.table(pred_full$score()) %>% select(measure, score)
as.data.table(pred_base$score())


# 创建解释器
explainer_lime_full <- lime::lime(test_full[, features_full], model_full, bin_continuous = FALSE)
explainer_lime_base <- lime::lime(test_base[, features_base], model_base, bin_continuous = FALSE)

# 解释第一个样本
explanation_full <- lime::explain(test_full[1, features_full], explainer_lime_full, n_features = 5)
explanation_base <- lime::explain(test_base[1, features_base], explainer_lime_base, n_features = 5)

# 可视化对比
plot_features(explanation_full, ncol = 1) + ggtitle("Full Model (with B data)")
plot_features(explanation_base, ncol = 1) + ggtitle("Base Model (without B data)")







