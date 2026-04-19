library(dplyr)
library(tibble)
library(data.table)
library(lubridate)
library(rlang)
library(janitor)
library(nlme)
library(AICcmodavg)
library(caret)
library(Metrics)
library(lmtest)

working_directory

# interrupted time series (ITS) using GLS - Weekly

## Period All
gls_model_all_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_vitamin %>%
    dplyr::filter(new_label == nn)
  
  ## Get values of p and q from AIC
  autocorrel = expand.grid(pval = 0:4, qval = 0:4)
  autocorrel_model <- sapply(1:nrow(autocorrel), function(x) {
    i <- x
    
    p <- try(summary(nlme::gls(value ~ weekly_time + intervention1 + post_intervention1_time +
                           intervention2 + post_intervention2_time
                         , data = df
                         , correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ weekly_time)
                         ,method="ML")
                     )$AIC
             , silent = TRUE
             )
    
    out <- as.data.frame(as.numeric(p))
    
  }, simplify = FALSE)
  
  autocorrel_model_merge <- data.table::rbindlist(autocorrel_model)[[1]]
  
  autocorrel_df <- autocorrel %>%
    dplyr::mutate(AIC = autocorrel_model_merge) %>%
    dplyr::slice_min(order_by = AIC, n=1, with_ties = FALSE)
  
  pval <- autocorrel_df$pval
  qval <- autocorrel_df$qval
  
  gls_method <- paste0("generalised least squares ", "accounting autocorrelation via corARMA", "(p=",pval, ", q=", qval, ")" )
  
  gls_model <- nlme::gls(value ~ weekly_time + intervention1 + post_intervention1_time +
                           intervention2 + post_intervention2_time
                         , data = df
                         , method = "ML" #REML, ML
                         , correlation = corARMA(p = pval, q = qval, form = ~ weekly_time, fixed = FALSE)
                         )
  
  ## Show a summary of the model
  summary_gls_model <- summary(gls_model)
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nutrient - vitamin"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
  ## Forecast the time series model
  gls_forecast <- predict(gls_model, df)
  
  ##Prediction uncertainities
  gls_forecast_se <- AICcmodavg::predictSE.gls(gls_model, df, se.fit=TRUE)
  
  prediction_uncertainties <- sapply(1:length(df$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = gls_forecast_se$fit[i]
                                 , sd = gls_forecast_se$se.fit[i]
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
    dplyr::mutate(across(everything(), ~round(.x, 5))
                  ) %>%
    rlang::set_names(~paste0(.x, "_all")) %>%
    dplyr::mutate(group = nn
                  , period = "all"
                  , actual = df$value
                  , date = df$week_date
                  )
  
  ## Model metrics
  model_metrics <- tibble::tibble(item = "nutrient - vitamin"
                                  , group = nn
                                  , period = "all"
                                  , method = paste0(gls_method, " fit by ", summary_gls_model[["method"]])
                                  , loglik = summary_gls_model[["logLik"]]
                                  , aic = summary_gls_model[["AIC"]]
                                  , bic = summary_gls_model[["BIC"]]
                                  , sigma2 = (summary_gls_model[["sigma"]])^2
                                  , data = "Training set"
                                  #, R2 = caret::R2(gls_forecast, df$value) #R2 = cor(df$value - gls_forecast)^2
                                  , me = mean((df$value - gls_forecast))
                                  #, mse = mean((df$value - gls_forecast)^2)
                                  , rmse = caret::RMSE(gls_forecast, df$value) #sqr(MSE)
                                  , mae = caret::MAE(gls_forecast, df$value) #MAE = mean(abs(df$value - gls_forecast))
                                  , mpe = mean((df$value - gls_forecast)*100/gls_forecast)
                                  , mape = mean(abs(df$value - gls_forecast)*100/gls_forecast)
                                  , mase = Metrics::mase(df$value, gls_forecast)
                                  )
  
  predict_df <- tibble::tibble(group = nn
                               , period = "all"
                               , actual = df$value
                               , date = df$week_date
                               , point_forecast_all = gls_forecast
                               , lo_95_all = gls_forecast - (1.96*gls_forecast_se$se.fit)
                               , hi_95_all = gls_forecast + (1.96*gls_forecast_se$se.fit)
                               )
  
  out <- list(model_metrics = model_metrics
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
}, simplify = FALSE
)

## Counterfactual - Period Interruption 1 (No interventions)
gls_model_interruption1_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_vitamin %>%
    dplyr::filter(new_label == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_start_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_start_week)
  
  ## Get values of p and q from AIC
  autocorrel = expand.grid(pval = 0:4, qval = 0:4)
  autocorrel_model <- sapply(1:nrow(autocorrel), function(x) {
    i <- x
    
    p <- try(summary(nlme::gls(value ~ weekly_time
                         , data = df_train
                         , correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ weekly_time)
                         ,method="ML")
                     )$AIC
             , silent = TRUE
             )
    
    out <- as.data.frame(as.numeric(p))
    
  }, simplify = FALSE)
  
  autocorrel_model_merge <- data.table::rbindlist(autocorrel_model)[[1]]
  
  autocorrel_df <- autocorrel %>%
    dplyr::mutate(AIC = autocorrel_model_merge) %>%
    dplyr::slice_min(order_by = AIC, n=1, with_ties = FALSE)
  
  pval <- autocorrel_df$pval
  qval <- autocorrel_df$qval
  
  gls_method <- paste0("generalised least squares ", "accounting autocorrelation via corARMA", "(p=",pval, ", q=", qval, ")" )
  
  gls_model <- nlme::gls(value ~ weekly_time
                         , data = df_train
                         , method = "ML" #REML, ML
                         , correlation = corARMA(p = pval, q = qval, form = ~ weekly_time, fixed = FALSE)
                         )
  
  ## Show a summary of the model
  summary_gls_model <- summary(gls_model)
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nutrient - vitamin"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
  ## Forecast the time series model
  gls_forecast <- predict(gls_model, df_test)
  
  ##Prediction uncertainities
  gls_forecast_se <- AICcmodavg::predictSE.gls(gls_model, df_test, se.fit=TRUE)
  
  prediction_uncertainties <- sapply(1:length(df_test$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = gls_forecast_se$fit[i]
                                 , sd = gls_forecast_se$se.fit[i]
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
    dplyr::mutate(across(everything(), ~round(.x, 5))
                  ) %>%
    rlang::set_names(~paste0(.x, "_interruption1")) %>%
    dplyr::mutate(group = nn
                  , period = "interruption 1"
                  , date = df_test$week_date
                  )
  
  ## Model metrics
  model_metrics <- tibble::tibble(item = "nutrient - vitamin"
                                  , group = nn
                                  , period = "interruption 1"
                                  , method = paste0(gls_method, " fit by ", summary_gls_model[["method"]])
                                  , loglik = summary_gls_model[["logLik"]]
                                  , aic = summary_gls_model[["AIC"]]
                                  , bic = summary_gls_model[["BIC"]]
                                  , sigma2 = (summary_gls_model[["sigma"]])^2
                                  , data = "Test set"
                                  #, R2 = caret::R2(gls_forecast, df_test$value) #R2 = cor(df_test$value - gls_forecast)^2
                                  , me = mean((df_test$value - gls_forecast))
                                  #, mse = mean((df_test$value - gls_forecast)^2)
                                  , rmse = caret::RMSE(gls_forecast, df_test$value) #sqr(MSE)
                                  , mae = caret::MAE(gls_forecast, df_test$value) #MAE = mean(abs(df_test$value - gls_forecast))
                                  , mpe = mean((df_test$value - gls_forecast)*100/gls_forecast)
                                  , mape = mean(abs(df_test$value - gls_forecast)*100/gls_forecast)
                                  , mase = Metrics::mase(df_test$value, gls_forecast)
                                  )
  
  predict_df <- tibble::tibble(group = nn
                               , period = "interruption 1"
                               , date = df_test$week_date
                               , point_forecast_interruption1 = gls_forecast
                               , lo_95_interruption1 = gls_forecast - (1.96*gls_forecast_se$se.fit)
                               , hi_95_interruption1 = gls_forecast + (1.96*gls_forecast_se$se.fit)
                               )
  
  out <- list(model_metrics = model_metrics
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
}, simplify = FALSE
)

## Counterfactual - Period Interruption 2 (Only interruption1)
gls_model_interruption2_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_vitamin %>%
    dplyr::filter(new_label == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_end_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_end_week)
  
  ## Get values of p and q from AIC
  autocorrel = expand.grid(pval = 0:4, qval = 0:4)
  autocorrel_model <- sapply(1:nrow(autocorrel), function(x) {
    i <- x
    
    p <- try(summary(nlme::gls(value ~ weekly_time + intervention1 + post_intervention1_time
                         , data = df_train
                         , correlation= corARMA(p=autocorrel$pval[i],q=autocorrel$qval[i], form = ~ weekly_time)
                         ,method="ML")
                     )$AIC
             , silent = TRUE
             )
    
    out <- as.data.frame(as.numeric(p))
    
  }, simplify = FALSE)
  
  autocorrel_model_merge <- data.table::rbindlist(autocorrel_model)[[1]]
  
  autocorrel_df <- autocorrel %>%
    dplyr::mutate(AIC = autocorrel_model_merge) %>%
    dplyr::slice_min(order_by = AIC, n=1, with_ties = FALSE)
  
  pval <- autocorrel_df$pval
  qval <- autocorrel_df$qval
  
  gls_method <- paste0("generalised least squares ", "accounting autocorrelation via corARMA", "(p=",pval, ", q=", qval, ")" )
  
  gls_model <- nlme::gls(value ~ weekly_time + intervention1 + post_intervention1_time
                         , data = df_train
                         , method = "ML" #REML, ML
                         , correlation = corARMA(p = pval, q = qval, form = ~ weekly_time, fixed = FALSE)
                         )
  
  ## Show a summary of the model
  summary_gls_model <- summary(gls_model)
  
  ## coefficients and pvalues
  coefficients_df <- dplyr::bind_cols(lmtest::coeftest(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame() %>%
                                        tibble::rownames_to_column(var = "term")
                                      , lmtest::coefci(gls_model#, df = summary_gls_model[["dims"]][["N"]]-1
                                                       , vcov. = summary_gls_model[["varBeta"]]
                                                       )[,] %>%
                                        as.data.frame()
                                      ) %>%
    dplyr::mutate(item = "nutrient - vitamin"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
  ## Forecast the time series model
  gls_forecast <- predict(gls_model, df_test)
  
  ##Prediction uncertainities
  gls_forecast_se <- AICcmodavg::predictSE.gls(gls_model, df_test, se.fit=TRUE)
  
  prediction_uncertainties <- sapply(1:length(df_test$week_date), function(y){
    i <- y
    set.seed(123)
    
    samples_norm <- stats::rnorm(num_samples
                                 , mean = gls_forecast_se$fit[i]
                                 , sd = gls_forecast_se$se.fit[i]
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
    dplyr::mutate(across(everything(), ~round(.x, 5))
                  ) %>%
    rlang::set_names(~paste0(.x, "_interruption2")) %>%
    dplyr::mutate(group = nn
                  , period = "interruption 2"
                  , date = df_test$week_date
                  )
  
  ## Model metrics
  model_metrics <- tibble::tibble(item = "nutrient - vitamin"
                                  , group = nn
                                  , period = "interruption 2"
                                  , method = paste0(gls_method, " fit by ", summary_gls_model[["method"]])
                                  , loglik = summary_gls_model[["logLik"]]
                                  , aic = summary_gls_model[["AIC"]]
                                  , bic = summary_gls_model[["BIC"]]
                                  , sigma2 = (summary_gls_model[["sigma"]])^2
                                  , data = "Test set"
                                  #, R2 = caret::R2(gls_forecast, df_test$value) #R2 = cor(df_test$value - gls_forecast)^2
                                  , me = mean((df_test$value - gls_forecast))
                                  #, mse = mean((df_test$value - gls_forecast)^2)
                                  , rmse = caret::RMSE(gls_forecast, df_test$value) #sqr(MSE)
                                  , mae = caret::MAE(gls_forecast, df_test$value) #MAE = mean(abs(df_test$value - gls_forecast))
                                  , mpe = mean((df_test$value - gls_forecast)*100/gls_forecast)
                                  , mape = mean(abs(df_test$value - gls_forecast)*100/gls_forecast)
                                  , mase = Metrics::mase(df_test$value, gls_forecast)
                                  )
  
  predict_df <- tibble::tibble(group = nn
                               , period = "interruption 2"
                               , date = df_test$week_date
                               , point_forecast_interruption2 = gls_forecast
                               , lo_95_interruption2 = gls_forecast - (1.96*gls_forecast_se$se.fit)
                               , hi_95_interruption2 = gls_forecast + (1.96*gls_forecast_se$se.fit)
                               )
  
  out <- list(model_metrics = model_metrics
              , coefficients = coefficients_df
              , predict_df = predict_df
              , predict_fit_df = prediction_uncertainties_df
              )
  
  
}, simplify = FALSE
)

