

library(jiebaR)
library(quanteda)
library(quanteda.textmodels)
library(dplyr)
library(ggplot2)

reviews <- read.csv("//Users//wulixin//Desktop//user_reviews.csv", header = TRUE, stringsAsFactors = FALSE)

bad_reviews<-reviews%>%filter(Rating<=3)

write.csv(bad_reviews,"//Users//wulixin//Desktop//bad_reviews.csv")

# 示例评论数据
docs <- reviews$Review
library(tmcn)
data("STOPWORDS")
# 初始化 jieba 分词器 + 加载自定义词典 + 停用词
segger <- worker(user = "//Users//wulixin//Desktop//custom_dict.txt", stop_word ='//Users//wulixin//Desktop//百度停用词表.txt')

# 分词函数
clean_jieba <- function(texts) {
  tokens <- lapply(texts, function(x) {
    if (!is.character(x)) x <- as.character(x)
    words <- segger <= gsub("[\\s\\d\\p{P}]", "", x)
    paste(words, collapse = " ")
  })
  return(unlist(tokens))
}

# 清洗 + 分词
reviews$tokens <- clean_jieba(docs)


#########################  3. 使用 quanteda 构建文档-词矩阵（DTM）
# 创建 corpus
my_corpus <- corpus(reviews$tokens)

# 构建 DFM（Document-Feature Matrix）
dfm <- dfm(tokens(my_corpus), remove_punct = TRUE, remove_numbers = TRUE)

# 查看高频词
topfeatures(dfm, 20)
library(topicmodels)
library(topicdoc)
library(quanteda)
library(quanteda.textmodels)

library(text2vec)

# 初始化迭代器
it <- itoken(reviews$Review, progressbar = FALSE)

# 构建词汇表
vocab <- create_vocabulary(it)

# 过滤低频词
vocab <- prune_vocabulary(vocab, term_count_min = 5)

# 构建 DTM
vectorizer <- vocab_vectorizer(vocab)
dtm_text2vec <- create_dtm(it, vectorizer)

# LDA 建模
lda_text2vec <- LDA(dtm_text2vec, k = 5, control = list(alpha = 0.1))

# 查看关键词
beta <- posterior(lda_text2vec)$terms
beta_df <- reshape2::melt(beta, value.name = "beta")
colnames(beta_df) <- c("term", "topic", "beta")

# 每个主题取前10个关键词
top_terms <- beta_df %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

############################3. 使用 stm 包做结构化主题模型（高级
library(stm)

# 准备数据
docvars <- data.frame(
  doc_id = rownames(dtm),
  rating = as.numeric(reviews$Rating),
  appname = reviews$AppName
)

# 构建 stm 模型
stm_model <- stm(
  documents = texts(dtm),
  vocab = colnames(dtm),
  K = 5,
  prevalence = ~ rating + appname,
  seed = 123
)

# 查看主题关键词
labelTopics(stm_model)

# 可视化
plot(stm_model)



##################### 4. 使用 LDA 进行主题建模（quanteda 接口）
# 训练 LDA 模型
lda_model <- textmodel_nb(dfm, k = 5)

# 查看每个主题的关键词
terms <- get_terms(lda_model, n = 10)

# 打印关键词
print(terms)


library(topicmodels)

# 设置随机种子
set.seed(123)

# 使用 VEM 算法训练 LDA 模型
#lda_model <- LDA(dfm, k = 5, method = "VEM")

# 如果你想使用 Gibbs Sampling
lda_model <- LDA(dtm, k = 5, method = "Gibbs")

# 提取每个主题的关键词
beta <- posterior(lda_model)$terms
beta_df <- reshape2::melt(beta, value.name = "beta")
colnames(beta_df) <- c("term", "topic", "beta")

# 每个主题取前10个关键词
top_terms <- beta_df %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)



library(ggplot2)

ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = as.factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(title = "LDA 主题模型关键词分布",
       x = "关键词",
       y = "概率值 (Beta)",
       fill = "主题编号") +
  theme_minimal()




################### 5. 使用 STM（Structural Topic Model）加入元信息（如 AppName, Rating） 
library(stm)
# 假设你已经有一个 dfm 对象
my_corpus <- dfm_array(dfm)  # 尝试提取原始语料库信息
raw_texts <- tokens_tolower(tokens_select(tokens(my_corpus), pattern = "*"))

# 或者更简单的方式：从 reviews 中重新提取原始评论
clean_texts <- reviews$Review %>%
  gsub("[\\p{P}\\d]", "", ., perl = TRUE)

# 再次 tokenize
stm_model <- stm(
  documents = tokenize(clean_texts),
  vocab = featnames(dfm),
  K = 5,
  prevalence = ~ rating + appname,
  docvar = docvars,
  seed = 123
)


# 查看主题关键词
labelTopics(stm_model)

# 可视化
plot(stm_model)

###################6. 使用 text2vec 构建 Word Co-Occurrence + LDA（高级）
library(text2vec)

# 初始化迭代器
it <- itoken(reviews$tokens, progressbar = FALSE)

# 构建词汇表
vocab <- create_vocabulary(it)

# 过滤低频词
vocab <- prune_vocabulary(vocab, term_count_min = 5)

# 构建 DTM
vectorizer <- vocab_vectorizer(vocab)
dtm_text2vec <- create_dtm(it, vectorizer)

# LDA 建模
lda_text2vec <- LDA(dtm_text2vec, k = 5, control = list(alpha = 0.1))

# 查看关键词
beta <- posterior(lda_text2vec)$terms
beta_df <- reshape2::melt(beta, value.name = "beta")
colnames(beta_df) <- c("term", "topic", "beta")

# 每个主题取前10个关键词
top_terms <- beta_df %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

####################可视化推荐

library(LDAvis)
library(LDAvis::serVis)


# 生成 LDAvis 数据
json_data <- createJSON(
  phi = lda_model$phi,
  theta = lda_model$theta,
  doc_term = as.matrix(dfm),
  vocab = featnames(dfm),
  doc_ids = docnames(dfm)
)

# 在浏览器中展示
serVis(json_data)




























