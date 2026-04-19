library(dplyr)
library(tibble)
library(data.table)
library(lubridate)
library(rlang)
library(janitor)
library(forecast)
library(lmtest)

working_directory

# interrupted time series (ITS) using ARIMA - Weekly

## Period All
arima_model_all_weekly_nova <- sapply(unique(df_analysis_ITS_weekly_nova$nova), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_nova %>%
    dplyr::filter(nova == nn)
  
  timeseries <- stats::ts(df$nova_prop, start = as.numeric(min(df$week_date)) 
                          , end = as.numeric(max(df$week_date))
                          , frequency = 1/7
                          )
  
  train_window <- stats::window(timeseries, start = as.numeric(min(df$week_date)))
  
  arima_model <- forecast::auto.arima(train_window
                                      , xreg = cbind(df$weekly_time,
                                                     df$intervention1, df$post_intervention1_time,
                                                     df$intervention2, df$post_intervention2_time)
                                      , stationary = FALSE
                                      , seasonal = TRUE
                                      , stepwise = FALSE
                                      , biasadj = FALSE
                                      )
  
  ##Ljung-Box test
  ljung_test <- stats::Box.test(arima_model$residuals)
  
  ljung_test_df <- tibble::tibble(item = "nova"
                                  , group = nn
                                  , period = "all"
                                  , method = ljung_test[["method"]]
                                  , statistic_X_squared = ljung_test[["statistic"]]
                                  , parameter_df = ljung_test[["parameter"]]
                                  , p_value = ljung_test[["p.value"]]
                                  )
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nova"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
  ## Forecast the time series model
  arima_forecast <- forecast::forecast(arima_model
                                       , h = length(df$week_date)
                                       , level = c(95)
                                       , biasadj = FALSE
                                       , xreg = cbind(df$weekly_time,
                                                      df$intervention1, df$post_intervention1_time,
                                                      df$intervention2, df$post_intervention2_time)
                                       )
  
  ##Prediction uncertainities
  arima_forecast_se <- predict(arima_model 
                               , n.ahead=length(df$week_date)
                               , newxreg = cbind(df$weekly_time,
                                                 df$intervention1, df$post_intervention1_time,
                                                 df$intervention2, df$post_intervention2_time)
                               ,se.fit=TRUE
                               )
  
  prediction_uncertainties <- sapply(1:length(df$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = arima_forecast_se[["pred"]][i]
                                 , sd = arima_forecast_se[["se"]][i]
                                 )
    
    point_forecast <- quantile(samples_norm, probs = 0.5, na.rm = TRUE)
    lo_95 <- quantile(samples_norm, probs = 0.025, na.rm = TRUE)
    hi_95 <- quantile(samples_norm, probs = 0.975, na.rm = TRUE)
    
    df <- tibble::tibble(point_forecast = point_forecast
                         ,lo_95 = lo_95
                         ,hi_95 = hi_95
                         )
    
  }, simplify = FALSE)
  
  prediction_uncertainties_df <- data.table::rbindlist(prediction_uncertainties) %>%
    dplyr::mutate(across(everything(), ~round(.x, 4))
                  ) %>%
    rlang::set_names(~paste0(.x, "_all")) %>%
    dplyr::mutate(group = nn
                  , period = "all"
                  , actual = df$nova_prop
                  , date = df$week_date
                  )
  
  ## Model metrics
  model_metrics <- dplyr::bind_cols(tibble::tibble( item = "nova"
                                                    , group = nn
                                                    , period = "all"
                                                    , method = arima_forecast[["method"]]
                                                    , loglik = arima_forecast[["model"]][["loglik"]]
                                                    , aic = arima_forecast[["model"]][["aic"]]
                                                    , bic = arima_forecast[["model"]][["bic"]]
                                                    , aicc = arima_forecast[["model"]][["aicc"]]
                                                    , sigma2 = arima_forecast[["model"]][["sigma2"]]
                                                    )
                                    , as.data.frame(forecast::accuracy(arima_forecast)) %>%
                                      janitor::clean_names() %>%
                                      tibble::rownames_to_column(var = "data")
                                    )
  
  predict_df <- dplyr::bind_cols(tibble::tibble(group = nn
                                              , period = "all"
                                              , actual = df$nova_prop
                                              , date = df$week_date
                                              )
                               , as.data.frame(arima_forecast) %>%
                                 tibble::remove_rownames() %>%
                                 janitor::clean_names() %>%
                                 dplyr::mutate(across(everything(), ~round(.x, 4))
                                               ) %>%
                                 rlang::set_names(~paste0(.x, "_all"))
                               )
  
  out <- list(model_metrics = model_metrics
              , ljung_test = ljung_test_df
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
  
}, simplify = FALSE
)

## Counterfactual - Period Interruption 1 (No interventions)
arima_model_interruption1_weekly_nova <- sapply(unique(df_analysis_ITS_weekly_nova$nova), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_nova %>%
    dplyr::filter(nova == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_start_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_start_week)
  
  timeseries <- stats::ts(df$nova_prop, start = as.numeric(min(df$week_date)) 
                          , end = as.numeric(max(df$week_date))
                          , frequency = 1/7
                          )
  
  train_window <- stats::window(timeseries, start = as.numeric(min(df_train$week_date))
                                , end = as.numeric(max(df_train$week_date))
                                )
  
  test_window <- stats::window(timeseries, start = as.numeric(min(df_test$week_date))
                               , end = as.numeric(max(df_test$week_date))
                               )
  
  arima_model <- forecast::auto.arima(train_window
                                      , xreg = cbind(df_train$weekly_time)
                                      , stationary = FALSE
                                      , seasonal = TRUE
                                      , stepwise = FALSE
                                      , biasadj = FALSE
                                      )
  
  ##Ljung-Box test
  ljung_test <- stats::Box.test(arima_model$residuals)
  
  ljung_test_df <- tibble::tibble(item = "nova"
                                  , group = nn
                                  , period = "interruption 1"
                                  , method = ljung_test[["method"]]
                                  , statistic_X_squared = ljung_test[["statistic"]]
                                  , parameter_df = ljung_test[["parameter"]]
                                  , p_value = ljung_test[["p.value"]]
                                  )
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nova"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item","group")))
  
  ## Forecast the time series model
  arima_forecast <- forecast::forecast(arima_model
                                       , h = length(df_test$week_date)
                                       , level = c(95)
                                       , biasadj = FALSE
                                       , xreg = cbind(df_test$weekly_time)
                                       )
  
  ##Prediction uncertainities
  arima_forecast_se <- predict(arima_model 
                               , n.ahead=length(df_test$week_date)
                               , newxreg = cbind(df_test$weekly_time)
                               ,se.fit=TRUE
                               )
  
  prediction_uncertainties <- sapply(1:length(df_test$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = arima_forecast_se[["pred"]][i]
                                 , sd = arima_forecast_se[["se"]][i]
                                 )
    
    point_forecast <- quantile(samples_norm, probs = 0.5, na.rm = TRUE)
    lo_95 <- quantile(samples_norm, probs = 0.025, na.rm = TRUE)
    hi_95 <- quantile(samples_norm, probs = 0.975, na.rm = TRUE)
    
    df <- tibble::tibble(point_forecast = point_forecast
                         ,lo_95 = lo_95
                         ,hi_95 = hi_95
                         )
    
  }, simplify = FALSE)
  
  prediction_uncertainties_df <- data.table::rbindlist(prediction_uncertainties) %>%
    dplyr::mutate(across(everything(), ~round(.x, 4))
                  ) %>%
    rlang::set_names(~paste0(.x, "_interruption1")) %>%
    dplyr::mutate(group = nn
                  , period = "interruption 1"
                  , date = df_test$week_date
                  )
  
  ## Model metrics
  model_metrics <- dplyr::bind_rows(tibble::tibble( item = "nova"
                                                    , group = nn
                                                    , period = "interruption 1"
                                                    , method = arima_forecast[["method"]]
                                                    , loglik = arima_forecast[["model"]][["loglik"]]
                                                    , aic = arima_forecast[["model"]][["aic"]]
                                                    , bic = arima_forecast[["model"]][["bic"]]
                                                    , aicc = arima_forecast[["model"]][["aicc"]]
                                                    , sigma2 = arima_forecast[["model"]][["sigma2"]]
                                                    )
                                    , as.data.frame(forecast::accuracy(arima_forecast, test_window)) %>%
                                      janitor::clean_names() %>%
                                      tibble::rownames_to_column(var = "data")
                                    )
  
  predict_df <- dplyr::bind_cols(tibble::tibble(group = nn
                                              , period = "interruption 1"
                                              , date = df_test$week_date
                                              )
                               , as.data.frame(arima_forecast) %>%
                                 tibble::remove_rownames() %>%
                                 janitor::clean_names() %>%
                                 dplyr::mutate(across(everything(), ~round(.x, 4))
                                               ) %>%
                                 rlang::set_names(~paste0(.x, "_interruption1"))
                               )
  
  out <- list(model_metrics = model_metrics
              , ljung_test = ljung_test_df
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
}, simplify = FALSE
)

## Counterfactual - Period Interruption 2 (Only interruption1)
arima_model_interruption2_weekly_nova <- sapply(unique(df_analysis_ITS_weekly_nova$nova), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_nova %>%
    dplyr::filter(nova == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_end_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_end_week)
  
  timeseries <- stats::ts(df$nova_prop, start = as.numeric(min(df$week_date)) 
                          , end = as.numeric(max(df$week_date))
                          , frequency = 1/7
                          )
  
  train_window <- stats::window(timeseries, start = as.numeric(min(df_train$week_date))
                                , end = as.numeric(max(df_train$week_date))
                                )
  
  test_window <- stats::window(timeseries, start = as.numeric(min(df_test$week_date)))
  
  arima_model <- forecast::auto.arima(train_window
                                      , xreg = cbind(df_train$weekly_time,
                                                     df_train$intervention1, df_train$post_intervention1_time)
                                      , stationary = FALSE
                                      , seasonal = TRUE
                                      , stepwise = FALSE
                                      , biasadj = FALSE
                                      )
  
  ##Ljung-Box test
  ljung_test <- stats::Box.test(arima_model$residuals)
  
  ljung_test_df <- tibble::tibble(item = "nova"
                                  , group = nn
                                  , period = "interruption 2"
                                  , method = ljung_test[["method"]]
                                  , statistic_X_squared = ljung_test[["statistic"]]
                                  , parameter_df = ljung_test[["parameter"]]
                                  , p_value = ljung_test[["p.value"]]
                                  )
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(arima_model#, df = arima_model$nobs-1
                                                       , vcov. = arima_model$var.coef)[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nova"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item","group")))
  
  ## Forecast the time series model
  arima_forecast <- forecast::forecast(arima_model
                                       , h = length(df_test$week_date)
                                       , level = c(95)
                                       , biasadj = FALSE
                                       , xreg = cbind(df_test$weekly_time,
                                                      df_test$intervention1, df_test$post_intervention1_time)
                                       )
  
  ##Prediction uncertainities
  arima_forecast_se <- predict(arima_model 
                               , n.ahead=length(df_test$week_date)
                               , newxreg = cbind(df_test$weekly_time,
                                                 df_test$intervention1, df_test$post_intervention1_time)
                               ,se.fit=TRUE
                               )
  
  prediction_uncertainties <- sapply(1:length(df_test$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = arima_forecast_se[["pred"]][i]
                                 , sd = arima_forecast_se[["se"]][i]
                                 )
    
    point_forecast <- quantile(samples_norm, probs = 0.5, na.rm = TRUE)
    lo_95 <- quantile(samples_norm, probs = 0.025, na.rm = TRUE)
    hi_95 <- quantile(samples_norm, probs = 0.975, na.rm = TRUE)
    
    df <- tibble::tibble(point_forecast = point_forecast
                         ,lo_95 = lo_95
                         ,hi_95 = hi_95
                         )
    
  }, simplify = FALSE)
  
  prediction_uncertainties_df <- data.table::rbindlist(prediction_uncertainties) %>%
    dplyr::mutate(across(everything(), ~round(.x, 4))
                  ) %>%
    rlang::set_names(~paste0(.x, "_interruption2")) %>%
    dplyr::mutate(group = nn
                  , period = "interruption 2"
                  , date = df_test$week_date
                  )
  
  ## Model metrics
  model_metrics <- dplyr::bind_rows(tibble::tibble( item = "nova"
                                                    , group = nn
                                                    , period = "interruption 2"
                                                    , method = arima_forecast[["method"]]
                                                    , loglik = arima_forecast[["model"]][["loglik"]]
                                                    , aic = arima_forecast[["model"]][["aic"]]
                                                    , bic = arima_forecast[["model"]][["bic"]]
                                                    , aicc = arima_forecast[["model"]][["aicc"]]
                                                    , sigma2 = arima_forecast[["model"]][["sigma2"]]
                                                    )
                                    , as.data.frame(forecast::accuracy(arima_forecast, test_window)) %>%
                                      janitor::clean_names() %>%
                                      tibble::rownames_to_column(var = "data")
                                    )
  
  predict_df <- dplyr::bind_cols(tibble::tibble(group = nn
                                              , period = "interruption 2"
                                              , date = df_test$week_date
                                              )
                               , as.data.frame(arima_forecast) %>%
                                 tibble::remove_rownames() %>%
                                 janitor::clean_names() %>%
                                 dplyr::mutate(across(everything(), ~round(.x, 4))
                                               ) %>%
                                 rlang::set_names(~paste0(.x, "_interruption2"))
                               )
  
  out <- list(model_metrics = model_metrics
              , ljung_test = ljung_test_df
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
}, simplify = FALSE
)

