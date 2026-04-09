library(dplyr)
library(tseries)
library(tibble)

working_directory

# Augmented Dickey–Fuller (ADF) t-statistic test - Weekly

## Find if series has a unit root. (series with a trend line will not have a unit root and result in a large p-value)

adf_ITS_weekly_proximate <- sapply(unique(df_analysis_ITS_weekly_proximate$new_label), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_proximate %>%
    dplyr::filter(new_label == nn)
  
  ## Augmented Dickey–Fuller (ADF) t-statistic test
  adf_test <- tseries::adf.test(df$value
                                , alternative = "stationary"
                                )
  
  adf_df <- tibble::tibble(item = "nutrient - proximate"
                           , group = nn
                           , method = adf_test[["method"]]
                           , hypothesis = adf_test[["alternative"]]
                           , statistic = adf_test[["statistic"]]
                           , parameter_lagorder = adf_test[["parameter"]]
                           , p_value = adf_test[["p.value"]]
                           )
  
}, simplify = FALSE
)

