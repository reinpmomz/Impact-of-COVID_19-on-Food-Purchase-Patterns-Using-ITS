library(dplyr)
library(grDevices)
library(lmtest)

working_directory

# Autocorrelation - Weekly

## Autocorrelation is a major issue when working with time series. 
## Autocorrelation occurs when observation at one point in time depends from observations at another point in time.
## When we use OLS, we assume that error terms associated with each observation are not correlated. 
## But, this assumption does not hold in the presence of autocorrelation because error terms are correlated across observations. 
## If you don’t correct for autocorrelation, you might underestimate the standard errors, meaning that you are overestimating the statistical significance.

grDevices::png(filename = base::file.path(output_Dir, paste0("acf_ITS_weekly_vitamin_plot", ".png")),
               height = 7,
               width = 10,
               units = "in",
               res = 300,
               bg = "white"
               )

par(mfrow = c(2, 4), cex.main = 0.8)

acf_ITS_weekly_vitamin <- sapply(unique(df_analysis_ITS_weekly_vitamin$new_label), function(x) {
  nn <- x
  
  df <- df_analysis_ITS_weekly_vitamin %>%
    dplyr::filter(new_label == nn)
  
  ## Durbin-Watson test to detect small correlation patterns
  acf_test <- lmtest::dwtest(df$value ~ df$weekly_time)
  acf_result <- paste0(acf_test[["method"]], " statistic = ", round(acf_test[["statistic"]], 3), "; p-value =",
                        ifelse(acf_test[["p.value"]] < 0.001,"<0.001", round(acf_test[["p.value"]], 3)
                               )
                       )
  ## Plot residuals
  acf_df <- acf(resid(lm(value ~ weekly_time, data = df)
                        )
                  , type = "correlation"
                  , plot = FALSE
                  )
  
  acf_plot <- plot(acf_df
                   , ci = 0.95
                   , xlab = "Lag"
                   , ylab = "ACF"
                   , main = paste0(nn,"\n", acf_result)
                   )
  
}, simplify = FALSE
)

# Close the PNG device
grDevices::dev.off()
