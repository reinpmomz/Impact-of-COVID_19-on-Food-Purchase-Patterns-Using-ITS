library(dplyr)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Inferential statistics - Chisquare/t-test
inferential_vars <- c("covid_period")

inferential_stats <- categorical_inferential_table(df = df_analysis_wide
                                                   ,by_vars = inferential_vars
                                                   ,foot_note = "n (%); Mean (SD); Median (IQR); Range"
                                                   ,caption = "Inferential Statistics"
                                                   ,include = names(df_analysis_wide)[!names(df_analysis_wide) 
                                                                                      %in% c("sdatetime", "week_number", "day_year", 
                                                                                             "month_name", "quarter_date", "year")]
                                                   ,categorical_proportion_digits = 2
                                                   ,continous_digits = 2
                                                   ,percent = "column" #default
                                                   ,p_value = TRUE #default
                                                   ,flex_table = TRUE
                                                   ,overall = TRUE
                                                   ,par_test = FALSE
                                                   ,var_equal = FALSE
                                                   )
print(inferential_stats)

