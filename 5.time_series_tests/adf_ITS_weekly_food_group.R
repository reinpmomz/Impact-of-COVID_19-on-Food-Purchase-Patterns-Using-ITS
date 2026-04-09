library(dplyr)
library(tseries)
library(tibble)

working_directory

# Augmented Dickey–Fuller (ADF) t-statistic test - Weekly

## Find if series has a unit root. (series with a trend line will not have a unit root and result in a large p-value)

adf_ITS_weekly_food_group <- sapply(unique(df_analysis_ITS_weekly_food_group$food_group), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_food_group %>%
    dplyr::filter(food_group == nn)
  
  ## Augmented Dickey–Fuller (ADF) t-statistic test
  adf_test <- tseries::adf.test(df$food_group_prop
                                , alternative = "stationary"
                                )
  
  adf_df <- tibble::tibble(item = "food group"
                           , group = nn
                           , method = adf_test[["method"]]
                           , hypothesis = adf_test[["alternative"]]
                           , statistic = adf_test[["statistic"]]
                           , parameter_lagorder = adf_test[["parameter"]]
                           , p_value = adf_test[["p.value"]]
                           )
  
}, simplify = FALSE
)

