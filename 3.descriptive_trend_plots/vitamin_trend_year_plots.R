library(dplyr)
library(tidyr)
library(ggplot2)


working_directory

ggtheme_descriptive_plot()

## Time series proportion plots
### Nutrient composition per year - Vitamins
nutrient_vitamin_vars <- c("vit_a_re_mcg", "thiamin_mg", "riboflavin_mg", "niacin_mg",
                           "dietary_folate_eq_mcg", "vit_b12_mcg", "vit_c_mg")

vitamin_monthly_year_plot <- df_clean_c_d %>%
  dplyr::select(year, month_name, any_of(nutrient_vitamin_vars)) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, month_name) %>%
  dplyr::summarise(across(any_of(nutrient_vitamin_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
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

print(vitamin_monthly_year_plot)


vitamin_quarterly_year_plot <- df_clean_c_d %>%
  dplyr::select(year, quarter_date, any_of(nutrient_vitamin_vars)) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, quarter_date) %>%
  dplyr::summarise(across(any_of(nutrient_vitamin_vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
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

print(vitamin_quarterly_year_plot)

## Saving the grid plots
ggsave(plot=vitamin_monthly_year_plot, height = 7, width = 13.5,
       filename = paste0("vitamin_monthly_year_plot",".png"),
       path = output_Dir, bg='white')

ggsave(plot=vitamin_quarterly_year_plot, height = 7, width = 11.5,
       filename = paste0("vitamin_quarterly_year_plot",".png"),
       path = output_Dir, bg='white')

