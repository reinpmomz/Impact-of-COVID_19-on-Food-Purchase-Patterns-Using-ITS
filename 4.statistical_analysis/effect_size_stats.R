library(dplyr)
library(forcats)


working_directory

#study period

effect_size_stats <- if (length(inferential_vars)>0) {
  
    effectsize_corr_table(df = df_analysis_wide %>%
                            dplyr::select(-c(sdatetime, week_number, day_year, month_name, quarter_date, year))
                          , by_vars = inferential_vars
                          , par_effsize = FALSE
                          , var_equal = FALSE
                          )
  
} else {
  print(paste0("No effect size analysis done. Select group variable"))
}
