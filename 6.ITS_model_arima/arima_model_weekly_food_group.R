library(dplyr)
library(tibble)
library(data.table)
library(lubridate)
library(rlang)
library(janitor)
library(forecast)
library(ggplot2)
library(lmtest)
library(ggpubr)

working_directory

# interrupted time series (ITS) using ARIMA - Weekly

## Period All
arima_model_all_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_food_group %>%
    dplyr::filter(food_group == nn)
  
  timeseries <- stats::ts(df$food_group_prop, start = as.numeric(min(df$week_date)) 
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
  
  ljung_test_df <- tibble::tibble(item = "food group"
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
    dplyr::mutate(item = "food group"
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
                               , n.ahead=length(df$week_date),
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
    dplyr::mutate(across(everything(), ~round(.x, 5))
                  ) %>%
    rlang::set_names(~paste0(.x, "_all")) %>%
    dplyr::mutate(group = nn
                  , period = "all"
                  , actual = df$food_group_prop
                  , date = df$week_date
                  )
  
  ## Model metrics
  model_metrics <- dplyr::bind_cols(tibble::tibble( item = "food group"
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
                                              , actual = df$food_group_prop
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
arima_model_interruption1_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_food_group %>%
    dplyr::filter(food_group == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_start_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_start_week)
  
  timeseries <- stats::ts(df$food_group_prop, start = as.numeric(min(df$week_date)) 
                          , end = as.numeric(max(df$week_date))
                          , frequency = 1/7
                          )
  
  train_window <- stats::window(timeseries, start = as.numeric(min(df_train$week_date))
                                , end = as.numeric(max(df_train$week_date))
                                )
  
  test_window <- stats::window(timeseries, start = as.numeric(min(df_test$week_date)))
  
  arima_model <- forecast::auto.arima(train_window
                                      , xreg = cbind(df_train$weekly_time)
                                      , stationary = FALSE
                                      , seasonal = TRUE
                                      , stepwise = FALSE
                                      , biasadj = FALSE
                                      )
  
  ##Ljung-Box test
  ljung_test <- stats::Box.test(arima_model$residuals)
  
  ljung_test_df <- tibble::tibble(item = "food group"
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
    dplyr::mutate(item = "food group"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
  ## Forecast the time series model
  arima_forecast <- forecast::forecast(arima_model
                                       , h = length(df_test$week_date)
                                       , level = c(95)
                                       , biasadj = FALSE
                                       , xreg = cbind(df_test$weekly_time)
                                       )
  
  ##Prediction uncertainities
  arima_forecast_se <- predict(arima_model 
                               , n.ahead=length(df_test$week_date),
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
  model_metrics <- dplyr::bind_rows(tibble::tibble( item = "food group"
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
arima_model_interruption2_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_food_group %>%
    dplyr::filter(food_group == nn)
  
  df_train <- df %>%
    dplyr::filter(week_date < curfew_end_week)
  
  df_test <- df %>%
    dplyr::filter(week_date >= curfew_end_week)
  
  timeseries <- stats::ts(df$food_group_prop, start = as.numeric(min(df$week_date)) 
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
  
  ljung_test_df <- tibble::tibble(item = "food group"
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
    dplyr::mutate(item = "food group"
                  , group = nn) %>%
    dplyr::relocate(any_of(c("item", "group")))
  
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
                               , n.ahead=length(df_test$week_date),
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
  model_metrics <- dplyr::bind_rows(tibble::tibble( item = "food group"
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

## ITS plot
df_arima_ITS_model_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- arima_model_all_weekly_food_group[[nn]][["predict_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(arima_model_interruption1_weekly_food_group[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(arima_model_interruption2_weekly_food_group[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)
  
arima_ITS_model_plot_weekly_food_group <- data.table::rbindlist(df_arima_ITS_model_weekly_food_group) %>%
  dplyr::mutate( label = if_else(date < curfew_start_day, "Pre-Covid",
                                          if_else(date < curfew_end_day, "Covid",
                                                  "Post-Covid"
                                                  )
                                          )
                 , label = factor(label, levels = c("Pre-Covid", "Covid", "Post-Covid") #covid_period to factor
                                  )
                 , curfew_start = curfew_start_day
                 , curfew_end = curfew_end_day
                 ) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(y_max = max(actual, point_forecast_interruption1, point_forecast_interruption2, na.rm = TRUE)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(x_min = min(date)
                , x_max = max(date)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(int_obj = lubridate::interval(x_min, x_max)
                , x_mid = lubridate::as_date(lubridate::int_start(int_obj) + (lubridate::int_end(int_obj) - lubridate::int_start(int_obj))/2)
                ) %>%
  ggplot(aes(x=date, y = actual)) +
  geom_rect(aes(xmin=x_min, xmax=x_max, ymin=-Inf, ymax=Inf, fill = label), show.legend = FALSE) +
  scale_fill_manual("", values = c("Pre-Covid" = "ivory1", "Covid" = "thistle1", "Post-Covid" = "lightcyan1")) +
  geom_text(aes(x= x_mid, y= y_max, label = label), vjust = 0.5, hjust = 0.5, size = 2.8, color="gray45") +
  geom_line(aes(colour="Observed"),lty=1)+
  geom_line(aes(y = point_forecast_all,colour="Regression line"),lty=1)+
  geom_line(aes(y = point_forecast_interruption1,colour="Counterfactual(No Interruption)"),lty=2) +
  geom_line(aes(y = point_forecast_interruption2,colour="Counterfactual(Covid-19 Interruption)"),lty=2) +
  geom_vline(aes(xintercept = curfew_start, colour = "Curfew Start"), linetype = "dotdash") +
  geom_vline(aes(xintercept = curfew_end, colour = "Curfew End"), linetype = "dotdash") +
  scale_colour_manual("", values = c("Observed" = "gray45", "Regression line" = "red",
                                     "Counterfactual(No Interruption)" = "green2",
                                     "Counterfactual(Covid-19 Interruption)" = "blue",
                                     "Curfew Start" = "purple",
                                     "Curfew End" = "orange"
                                     )
                    ) +
  scale_y_continuous( labels = scales::percent, n.breaks = 5, expand = expansion(mult = c(0.05,0.05))
                      ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01))
                ) +
  labs(x= "Time(Weeks)", y = "Proportion of Food Purchases") +
  guides(colour=guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
    axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
    axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
    axis.ticks.y = element_line(linewidth = 0.5, color="black"),
    axis.ticks.x = element_line(linewidth = 0.5, color="black"),
    axis.text.y = element_text(lineheight = 0.7, hjust = 0.5, size = 9),
    axis.text.x = element_text(angle = 90, lineheight = 0.7, vjust = 0.5, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.caption = element_text(angle = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold"),
    #strip.background.x = element_rect(fill = "grey80", linetype = 0),
    #strip.background.y = element_rect(fill = "grey80", linetype = 0), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +
  facet_wrap(~group, ncol = 4, scales = "free_y")
    

### Saving the ITS plots
ggsave(plot=arima_ITS_model_plot_weekly_food_group, height = 7.5, width = 13.5,
       filename = paste0("arima_ITS_plot_weekly_food_group",".png"),
       path = output_Dir, bg='white')


## Prediction plot
df_arima_prediction_model_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- arima_model_all_weekly_food_group[[nn]][["predict_fit_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(arima_model_interruption1_weekly_food_group[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(arima_model_interruption2_weekly_food_group[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)

arima_prediction_model_plot_weekly_food_group <- data.table::rbindlist(df_arima_prediction_model_weekly_food_group) %>%
  dplyr::mutate(curfew_start = curfew_start_day
                , curfew_end = curfew_end_day
                ) %>%
  ggplot(aes(x=date, y = actual)) +
  geom_line(aes(y = point_forecast_interruption1,colour="Predicted(No Interruption)"),lty=1) +
  geom_line(aes(y = point_forecast_interruption2,colour="Predicted(Covid-19 Interruption)"),lty=1) +
  geom_ribbon(aes(ymin = lo_95_interruption1, ymax = hi_95_interruption1, fill = "95% CI Predicted(No Interruption)"), alpha=0.2)+
  geom_ribbon(aes(ymin = lo_95_interruption2, ymax = hi_95_interruption2, fill = "95% CI Predicted(Covid-19 Interruption)"), alpha=0.2)+
  geom_line(aes(color="Observed"),lty=1)+
  geom_vline(aes(xintercept = curfew_start, colour = "Curfew Start"), linetype = "dotdash") +
  geom_vline(aes(xintercept = curfew_end, colour = "Curfew End"), linetype = "dotdash") +
  scale_colour_manual("", values = c("Observed" = "gray45",
                                     "Predicted(No Interruption)" = "green2",
                                     "Predicted(Covid-19 Interruption)" = "blue",
                                     "Curfew Start" = "purple",
                                     "Curfew End" = "orange"
                                     )
                    ) +
  scale_fill_manual("", values = c("95% CI Predicted(No Interruption)" = "green2", "95% CI Predicted(Covid-19 Interruption)" = "blue")) +
  scale_y_continuous( labels = scales::percent, n.breaks = 5, expand = expansion(mult = c(0.05,0.05))
                      ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01))
                ) +
  labs(x= "Time(Weeks)", y = "Proportion of Food Purchases") +
  guides(colour=guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
    axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
    axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
    axis.ticks.y = element_line(linewidth = 0.5, color="black"),
    axis.ticks.x = element_line(linewidth = 0.5, color="black"),
    axis.text.y = element_text(lineheight = 0.7, hjust = 0.5, size = 9),
    axis.text.x = element_text(angle = 90, lineheight = 0.7, vjust = 0.5, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.caption = element_text(angle = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold"),
    #strip.background.x = element_rect(fill = "grey80", linetype = 0),
    #strip.background.y = element_rect(fill = "grey80", linetype = 0), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +
  facet_wrap(~group, ncol = 4, scales = "free_y")

### Saving the Prediction plots
ggsave(plot=arima_prediction_model_plot_weekly_food_group, height = 7.5, width = 13.5,
       filename = paste0("arima_prediction_plot_weekly_food_group",".png"),
       path = output_Dir, bg='white')

