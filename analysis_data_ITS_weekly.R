library(dplyr)
library(tidyr)
library(lubridate)
library(fastDummies)
library(tibble)

working_directory

# dataset with variables for Uncontrolled Interuppted Time series, two interventions - Weekly

## checking if all weeks are there
print(length(unique(lubridate::floor_date(df_analysis$sdatetime, unit = "week", week_start = 1)
                    )
             )
      )  

print(length(seq(from = min(df_analysis$sdatetime)
                 ,to = max(df_analysis$sdatetime)
                 , by = "week"
                 )
             )
      )

## Align start day of week with end day of week
curfew_start_week <- lubridate::floor_date(curfew_start_day,
                                           unit = "week",
                                           week_start = lubridate::wday(curfew_start_day, week_start = 1
                                                                        ) #week starts on day of week of curfew start date
                                           ) 

curfew_end_week <- lubridate::ceiling_date(curfew_end_day, 
                                           unit = "week",
                                           week_start = lubridate::wday(curfew_start_day, week_start = 1
                                                                        ) #week ends on day of week of curfew start date
                                           )

# data frame of all weeks and dummy quarters
weeks_df <- df_analysis %>%
  dplyr::distinct(sdatetime) %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date
                , quarter = lubridate::quarter(sdatetime, type = "quarter") #numeric quarter
                ) %>%
  fastDummies::dummy_cols(select_columns = c("quarter")
                          ,ignore_na = TRUE #ignores any NA values in the column
                          , remove_selected_columns = FALSE #removes the columns used to generate the dummy columns.
                          , omit_colname_prefix = FALSE
                          ) %>%
  dplyr::select(-sdatetime) %>%
  dplyr::distinct(week_date, .keep_all = TRUE)

weeks_no_df <- df_analysis %>%
  dplyr::distinct(sdatetime) %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date
                ) %>%
  dplyr::distinct(week_date) %>%
  dplyr::mutate(period = if_else(week_date <curfew_start_week, "Pre-Covid",
                                 if_else(week_date <curfew_end_week, "Covid", "Post-Covid"
                                         )
                                 )
                ) %>%
  dplyr::group_by(period) %>%
  dplyr::count(name = "no. of weeks")
  

#data frame of all weeks with nova groups
weeks_df_nova <- tibble::tibble(week_date = rep(weeks_df$week_date, each = length(unique(df_analysis$nova))
                                                )
                                ,nova = rep(unique(df_analysis$nova), times = length(unique(weeks_df$week_date))
                                                )
                                ) %>%
  tidyr::drop_na(nova)

#data frame of all weeks with food groups
weeks_df_food_group <- tibble::tibble(week_date = rep(weeks_df$week_date, each = length(unique(df_analysis$food_group))
                                                )
                                ,food_group = rep(unique(df_analysis$food_group), times = length(unique(weeks_df$week_date))
                                                )
                                ) %>%
  tidyr::drop_na(food_group)


## nova
df_analysis_ITS_weekly_nova <- df_analysis %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date plus 1
                ) %>%
  tidyr::drop_na(nova) %>%
  dplyr::group_by(week_date) %>%
  dplyr::mutate(total = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(across(c(week_date, total, nova))) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(nova_prop = round(count/total, 4)) %>%
  dplyr::right_join(weeks_df_nova, by = c("week_date", "nova")) %>%
  tidyr::replace_na(list(total = 0, count = 0, nova_prop = 0)) %>%
  dplyr::mutate(intervention1 = if_else(week_date < curfew_start_week, 0, 1)
                , intervention2 = if_else(week_date < curfew_end_week, 0, 1)
                ) %>%
  dplyr::group_by(nova) %>%
  dplyr::mutate(weekly_time = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(nova, intervention1) %>%
  dplyr::mutate(post_intervention1_time = row_number()
                , post_intervention1_time = if_else(intervention1 == 0, 0, post_intervention1_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(nova, intervention2) %>%
  dplyr::mutate(post_intervention2_time = row_number()
                , post_intervention2_time = if_else(intervention2 == 0, 0, post_intervention2_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(nova, nova_prop, week_date, weekly_time, intervention1, post_intervention1_time,
                intervention2, post_intervention2_time) %>%
  dplyr::left_join(weeks_df, by = c("week_date"))

## food group
df_analysis_ITS_weekly_food_group <- df_analysis %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date plus 1
                ) %>%
  tidyr::drop_na(food_group) %>%
  dplyr::group_by(week_date) %>%
  dplyr::mutate(total = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(across(c(week_date, total, food_group))) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(food_group_prop = round(count/total, 4)) %>%
  dplyr::right_join(weeks_df_food_group, by = c("week_date", "food_group")) %>%
  tidyr::replace_na(list(total = 0, count = 0, food_group_prop = 0)) %>%
  dplyr::mutate(intervention1 = if_else(week_date < curfew_start_week, 0, 1)
                , intervention2 = if_else(week_date < curfew_end_week, 0, 1)
                ) %>%
  dplyr::group_by(food_group) %>%
  dplyr::mutate(weekly_time = row_number()) %>%
  dplyr::group_by(food_group, intervention1) %>%
  dplyr::mutate(post_intervention1_time = row_number()
                , post_intervention1_time = if_else(intervention1 == 0, 0, post_intervention1_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(food_group, intervention2) %>%
  dplyr::mutate(post_intervention2_time = row_number()
                , post_intervention2_time = if_else(intervention2 == 0, 0, post_intervention2_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(food_group, food_group_prop, week_date, weekly_time, intervention1, post_intervention1_time,
                intervention2, post_intervention2_time) %>%
  dplyr::left_join(weeks_df, by = c("week_date"))

## Food composition - Proximates
df_analysis_ITS_weekly_proximate <- df_analysis %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date plus 1
                ) %>%
  dplyr::group_by(week_date) %>%
  dplyr::summarise(across(any_of(nutrient_proximate_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = !c(week_date)
                      ,names_to = "nutrient"
                      , values_to = "value"
                      ) %>%
  dplyr::left_join(rename_vars_df %>% dplyr::select(new_variable, new_label)
                   , by = c("nutrient" = "new_variable")
                   ) %>%
  dplyr::mutate(intervention1 = if_else(week_date < curfew_start_week, 0, 1)
                , intervention2 = if_else(week_date < curfew_end_week, 0, 1)
                ) %>%
  dplyr::group_by(new_label) %>%
  dplyr::mutate(weekly_time = row_number()) %>%
  dplyr::group_by(new_label, intervention1) %>%
  dplyr::mutate(post_intervention1_time = row_number()
                , post_intervention1_time = if_else(intervention1 == 0, 0, post_intervention1_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(new_label, intervention2) %>%
  dplyr::mutate(post_intervention2_time = row_number()
                , post_intervention2_time = if_else(intervention2 == 0, 0, post_intervention2_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(new_label, value, week_date, weekly_time, intervention1, post_intervention1_time,
                intervention2, post_intervention2_time) %>%
  dplyr::left_join(weeks_df, by = c("week_date"))

## Food composition - Minerals
df_analysis_ITS_weekly_mineral <- df_analysis %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date plus 1
                ) %>%
  dplyr::group_by(week_date) %>%
  dplyr::summarise(across(any_of(nutrient_mineral_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = !c(week_date)
                      ,names_to = "nutrient"
                      , values_to = "value"
                      ) %>%
  dplyr::left_join(rename_vars_df %>% dplyr::select(new_variable, new_label)
                   , by = c("nutrient" = "new_variable")
                   ) %>%
  dplyr::mutate(intervention1 = if_else(week_date < curfew_start_week, 0, 1)
                , intervention2 = if_else(week_date < curfew_end_week, 0, 1)
                ) %>%
  dplyr::group_by(new_label) %>%
  dplyr::mutate(weekly_time = row_number()) %>%
  dplyr::group_by(new_label, intervention1) %>%
  dplyr::mutate(post_intervention1_time = row_number()
                , post_intervention1_time = if_else(intervention1 == 0, 0, post_intervention1_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(new_label, intervention2) %>%
  dplyr::mutate(post_intervention2_time = row_number()
                , post_intervention2_time = if_else(intervention2 == 0, 0, post_intervention2_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(new_label, value, week_date, weekly_time, intervention1, post_intervention1_time,
                intervention2, post_intervention2_time) %>%
  dplyr::left_join(weeks_df, by = c("week_date"))

## Food composition - Vitamins
df_analysis_ITS_weekly_vitamin <- df_analysis %>%
  dplyr::mutate(week_date = lubridate::floor_date(sdatetime, unit = "week",
                                                  week_start = lubridate::wday(curfew_start_day, week_start = 1)
                                                  ) #week starts on day of week of curfew start date
                ) %>%
  dplyr::group_by(week_date) %>%
  dplyr::summarise(across(any_of(nutrient_vitamin_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = !c(week_date)
                      ,names_to = "nutrient"
                      , values_to = "value"
                      ) %>%
  dplyr::left_join(rename_vars_df %>% dplyr::select(new_variable, new_label)
                   , by = c("nutrient" = "new_variable")
                   ) %>%
  dplyr::mutate(intervention1 = if_else(week_date < curfew_start_week, 0, 1)
                , intervention2 = if_else(week_date < curfew_end_week, 0, 1)
                ) %>%
  dplyr::group_by(new_label) %>%
  dplyr::mutate(weekly_time = row_number()) %>%
  dplyr::group_by(new_label, intervention1) %>%
  dplyr::mutate(post_intervention1_time = row_number()
                , post_intervention1_time = if_else(intervention1 == 0, 0, post_intervention1_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(new_label, intervention2) %>%
  dplyr::mutate(post_intervention2_time = row_number()
                , post_intervention2_time = if_else(intervention2 == 0, 0, post_intervention2_time)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(new_label, value, week_date, weekly_time, intervention1, post_intervention1_time,
                intervention2, post_intervention2_time) %>%
  dplyr::left_join(weeks_df, by = c("week_date"))

