library(dplyr)
library(tidyr)
library(ggplot2)


working_directory

ggtheme_descriptive_plot()

## Time series proportion plots
### Nutrient composition per year - Proximates
nutrient_proximate_vars <- c("energy_kcal", "water_g", "protein_g", "fat_g", "carbohydrate_available_g",
                             "fibre_g", "cholesterol_chole_mg")

proximate_monthly_year_plot <- df_clean_c_d %>%
  dplyr::select(year, month_name, any_of(nutrient_proximate_vars)) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, month_name) %>%
  dplyr::summarise(across(any_of(nutrient_proximate_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = !c(year, month_name)
                      ,names_to = "nutrient"
                      , values_to = "value"
                      ) %>%
  dplyr::left_join(rename_vars_df %>% dplyr::select(new_variable, new_label)
                   , by = c("nutrient" = "new_variable")
                   ) %>%
  line_group_sum_mean_grid_plot(x_vars= c("month_name")
                                , y_vars = c("value")
                                , x_label = "Time(Months)"
                                , y_label = "Nutrient values per 100g/100ml"
                                , colour_vars= c("year")
                                , title_label = ""
                                , y_axis_breaks =6
                                , y_axis_label_percent = FALSE #default
                                , y_axis_limits = c(0, NA)
                                , facet_vars=c("new_label")
                                , facet_wrap=TRUE
                                )

print(proximate_monthly_year_plot)


proximate_quarterly_year_plot <- df_clean_c_d %>%
  dplyr::select(year, quarter_date, any_of(nutrient_proximate_vars)) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, quarter_date) %>%
  dplyr::summarise(across(any_of(nutrient_proximate_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(cols = !c(year, quarter_date)
                      ,names_to = "nutrient"
                      , values_to = "value"
                      ) %>%
  dplyr::left_join(rename_vars_df %>% dplyr::select(new_variable, new_label)
                   , by = c("nutrient" = "new_variable")
                   ) %>%
  line_group_sum_mean_grid_plot(x_vars= c("quarter_date")
                                , y_vars = c("value")
                                , x_label = "Time(Quarter)"
                                , y_label = "Nutrient values per 100g/100ml"
                                , colour_vars= c("year")
                                , title_label = ""
                                , y_axis_breaks =6
                                , y_axis_label_percent = FALSE #default
                                , y_axis_limits = c(0, NA)
                                , facet_vars=c("new_label")
                                , facet_wrap=TRUE
                                )

print(proximate_quarterly_year_plot)

## Saving the grid plots
ggsave(plot=proximate_monthly_year_plot, height = 7, width = 13.5,
       filename = paste0("proximate_monthly_year_plot",".png"),
       path = output_Dir, bg='white')

ggsave(plot=proximate_quarterly_year_plot, height = 7, width = 11.5,
       filename = paste0("proximate_quarterly_year_plot",".png"),
       path = output_Dir, bg='white')

