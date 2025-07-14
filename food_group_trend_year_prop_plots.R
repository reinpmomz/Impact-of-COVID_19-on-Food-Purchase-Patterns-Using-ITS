library(dplyr)
library(tidyr)
library(ggplot2)


working_directory

ggtheme_descriptive_plot()

## Time series proportion plots
### Food groups per year
food_group_monthly_year_prop_plot <- df_clean_c_d %>%
  dplyr::select(food_group, year, month_name) %>%
  tidyr::drop_na(food_group) %>%
  dplyr::filter(!food_group %in% unique(drop_selected_levels_df$level[drop_selected_levels_df$variable == "food_group"])
                ) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, month_name) %>%
  dplyr::mutate(total = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(across(c(year, month_name, total, food_group))) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(prop = round(count/total, 3)) %>%
  line_group_sum_mean_grid_plot(x_vars= c("month_name")
                                , y_vars = c("prop")
                                , x_label = "Time(Months)"
                                , y_label = "Proportion of Food Purchases"
                                , colour_vars= c("year")
                                , title_label = ""
                                , y_axis_breaks =6
                                , y_axis_label_percent = TRUE
                                , y_axis_limits = c(0, NA)
                                , facet_vars=c("food_group")
                                , facet_wrap=TRUE
                                , facet_ncol = 4
                                )

print(food_group_monthly_year_prop_plot)


food_group_quarterly_year_prop_plot <- df_clean_c_d %>%
  dplyr::select(food_group, year, quarter_date) %>%
  tidyr::drop_na(food_group) %>%
  dplyr::filter(!food_group %in% unique(drop_selected_levels_df$level[drop_selected_levels_df$variable == "food_group"])
                ) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  dplyr::group_by(year, quarter_date) %>%
  dplyr::mutate(total = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(across(c(year, quarter_date, total, food_group))) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::mutate(prop = round(count/total, 3)) %>%
  line_group_sum_mean_grid_plot(x_vars= c("quarter_date")
                                , y_vars = c("prop")
                                , x_label = "Time(Quarter)"
                                , y_label = "Proportion of Food Group Purchases"
                                , colour_vars= c("year")
                                , title_label = ""
                                , y_axis_breaks =6
                                , y_axis_label_percent = TRUE
                                , y_axis_limits = c(0, NA)
                                , facet_vars=c("food_group")
                                , facet_wrap=TRUE
                                , facet_ncol = 4
                                )

print(food_group_quarterly_year_prop_plot)

## Saving the grid plots
ggsave(plot=food_group_monthly_year_prop_plot, height = 7, width = 13.5,
       filename = paste0("food_group_monthly_year_prop_plot",".png"),
       path = output_Dir, bg='white')

ggsave(plot=food_group_quarterly_year_prop_plot, height = 7, width = 13.5,
       filename = paste0("food_group_quarterly_year_prop_plot",".png"),
       path = output_Dir, bg='white')

