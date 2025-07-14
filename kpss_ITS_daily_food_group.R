library(dplyr)
library(tseries)
library(tibble)

working_directory

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity - Daily

## Test if the time series is level or trend stationary. 
## Test the null hypothesis of trend stationarity 
## (low p-value will indicate a signal that is not trend stationary, has a unit root)
## "level stationary" - a time series where the mean (average) of the series is constant over time
## "trend stationary" - a time series where the mean changes in a predictable, deterministic way (a trend) over time, and the deviations from that trend are stationary.

kpss_ITS_daily_food_group <- sapply(unique(df_analysis_ITS_daily_food_group$food_group), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_daily_food_group %>%
    dplyr::filter(food_group == nn)
  
  ## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
  kpss_level_test <- tseries::kpss.test(df$food_group_prop
                                        , null = "Level"
                                        )
  
  kpss_trend_test <- tseries::kpss.test(df$food_group_prop
                                        , null = "Trend"
                                        )
  
  kpss_level_df <- tibble::tibble(item = "food group"
                                  , group = nn
                                  , method = kpss_level_test[["method"]]
                                  , statistic = kpss_level_test[["statistic"]]
                                  , parameter_truncationlag = kpss_level_test[["parameter"]]
                                  , p_value = kpss_level_test[["p.value"]]
                                  )
  
  kpss_trend_df <- tibble::tibble(item = "food group"
                                  , group = nn
                                  , method = kpss_trend_test[["method"]]
                                  , statistic = kpss_trend_test[["statistic"]]
                                  , parameter_truncationlag = kpss_trend_test[["parameter"]]
                                  , p_value = kpss_trend_test[["p.value"]]
                                  )
  
  kpss_merge_df <- dplyr::bind_rows(kpss_level_df, kpss_trend_df)
  
}, simplify = FALSE
)

