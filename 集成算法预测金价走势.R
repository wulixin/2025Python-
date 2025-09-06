#########################################################
#
#            Ensemble Model for Gold Futures
#
#########################################################

library(tidymodels)
library(tidyverse)
library(modeltime)
library(modeltime.ensemble)
library(timetk)
library(tidyquant)
library(Quandl)

# 设置你的 API Key（免费注册：https://data.nasdaq.com/signup）
Quandl.api_key('hcbddB18XwJRQkMm4yKd')
data = Quandl("LBMA/GOLD")
plot(data[,1])


# 获取黄金现货价格
data = quandl.get()
print(data.tail())  # 查看最新数据



#Gold Futures
df_gold <- 
  tq_get("GC=F") %>% 
  select(date, close) %>% 
  drop_na()


#Splitting
splits <- 
  time_series_split(df_gold, 
                    assess = "30 days", 
                    cumulative = TRUE)

df_train <- training(splits)
df_test <- testing(splits)

#Recipe
rec_spec <- 
  recipe(close ~ ., data = df_train) %>% 
  step_timeseries_signature(date) %>% 
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())


#Model 1 - Auto ARIMA
mod_spec_arima <- 
  arima_reg() %>%
  set_engine("auto_arima")

wflw_fit_arima <- 
  workflow() %>%
  add_model(mod_spec_arima) %>%
  add_recipe(rec_spec %>% step_rm(all_predictors(), -date)) %>%
  fit(df_train)

#Model 2 - Prophet
mod_spec_prophet <- 
  prophet_reg() %>%
  set_engine("prophet")

wflw_fit_prophet <- 
  workflow() %>%
  add_model(mod_spec_prophet) %>%
  add_recipe(rec_spec) %>%
  fit(df_train)

#Model 3: Boosted ARIMA
mod_arima_boosted <- 
  arima_boost(
    min_n = 2,
    learn_rate = 0.015
  ) %>%
  set_engine(engine = "auto_arima_xgboost")

wflw_fit_arima_boost <- 
  workflow() %>%
  add_model(mod_arima_boosted) %>%
  add_recipe(rec_spec) %>%
  fit(df_train)

#Modeltime Workflow for Ensemble Forecasting
df_models <- 
  modeltime_table(
    wflw_fit_arima,
    wflw_fit_prophet,
    wflw_fit_arima_boost
  )


#Make an Ensemble
ensemble_fit <- 
  df_models %>%
  ensemble_average(type = "mean")

#Calibration
calibration_tbl <- 
  modeltime_table(
    ensemble_fit
  ) %>%
  modeltime_calibrate(df_test)


#Accuracy
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = TRUE
  )


#Predictive intervals
calibration_tbl %>%
  modeltime_forecast(actual_data = df_test,
                     new_data = df_test) %>%
  plot_modeltime_forecast(.interactive = FALSE,
                          .legend_show = FALSE,
                          .line_size = 1.5,
                          .color_lab = "",
                          .title = "Gold Futures") +
  labs(subtitle = "<span style = 'color:dimgrey;'>Predictive Intervals</span> of the <span style = 'color:red;'>Ensemble Model</span>") + 
  scale_y_continuous(labels = scales::label_currency()) +
  scale_x_date(labels = scales::label_date("%b %d"),
               date_breaks = "4 days") +
  theme_minimal(base_family = "Roboto Slab", base_size = 16) +
  theme(plot.subtitle = ggtext::element_markdown(face = "bold"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "azure", color = "azure"),
        panel.background = element_rect(fill = "snow", color = "snow"),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1),
        legend.position = "none")

