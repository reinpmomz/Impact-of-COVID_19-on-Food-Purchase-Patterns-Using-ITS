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
library(ggplot2)
library(lmtest)
library(ggpubr)

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

## ITS plot
df_gls_ITS_model_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- gls_model_all_weekly_vitamin[[nn]][["predict_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(gls_model_interruption1_weekly_vitamin[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(gls_model_interruption2_weekly_vitamin[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)
  
gls_ITS_model_plot_weekly_vitamin <- data.table::rbindlist(df_gls_ITS_model_weekly_vitamin) %>%
  dplyr::mutate( label = if_else(date < curfew_start_day, "Pre-Covid",
                                          if_else(date < curfew_end_day, "Covid",
                                                  "Post-Covid"
                                                  )
                                          )
                 , label = factor(label, levels = c("Pre-Covid", "Covid", "Post-Covid") #covid_period to factor
                                  )
                 , curfew_start = curfew_start_day
                 , curfew_end = curfew_end_day
                 , group = forcats::as_factor(group) #creates levels in the order in which they appear
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
  scale_y_continuous( n.breaks = 7, expand = expansion(mult = c(0.05,0.05)) ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01)) ) +
  labs(x= "Time(Weeks)", y = "Nutrient values per 100g/100ml") +
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
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold"),
    #strip.background.x = element_rect(fill = "grey80", linetype = 0),
    #strip.background.y = element_rect(fill = "grey80", linetype = 0), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +
  facet_wrap(~group, ncol = 2, scales = "free_y")
    
### Saving the ITS plots
ggsave(plot=gls_ITS_model_plot_weekly_vitamin, height = 7.5, width = 13.5,
       filename = paste0("gls_ITS_plot_weekly_vitamin",".png"),
       path = output_Dir, bg='white')


## Prediction plot
df_gls_prediction_model_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- gls_model_all_weekly_vitamin[[nn]][["predict_fit_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(gls_model_interruption1_weekly_vitamin[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(gls_model_interruption2_weekly_vitamin[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)


gls_prediction_model_plot_weekly_vitamin <- data.table::rbindlist(df_gls_prediction_model_weekly_vitamin) %>%
  dplyr::mutate(curfew_start = curfew_start_day
                , curfew_end = curfew_end_day
                , group = forcats::as_factor(group) #creates levels in the order in which they appear
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
  scale_y_continuous( n.breaks = 7, expand = expansion(mult = c(0.05,0.05)) ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01)) ) +
  labs(x= "Time(Weeks)", y = "Nutrient values per 100g/100ml") +
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
  facet_wrap(~group, ncol = 2, scales = "free_y")

### Saving the Prediction plots
ggsave(plot=gls_prediction_model_plot_weekly_vitamin, height = 7.5, width = 13.5,
       filename = paste0("gls_prediction_plot_weekly_vitamin",".png"),
       path = output_Dir, bg='white')

