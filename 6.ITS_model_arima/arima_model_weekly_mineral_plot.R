library(dplyr)
library(scales)
library(data.table)
library(lubridate)
library(ggplot2)

working_directory

## ITS plot
df_arima_ITS_model_weekly_mineral <- sapply(unique(df_analysis_ITS_weekly_mineral$new_label), function(x) {
  nn <- x
  
  df <- arima_model_all_weekly_mineral[[nn]][["predict_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(arima_model_interruption1_weekly_mineral[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(arima_model_interruption2_weekly_mineral[[nn]][["predict_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)
  
arima_ITS_model_plot_weekly_mineral <- data.table::rbindlist(df_arima_ITS_model_weekly_mineral) %>%
  dplyr::mutate( label = if_else(date < curfew_start_day, "Pre-Covid",
                                          if_else(date < curfew_end_day, "Covid",
                                                  "Post-Covid"
                                                  )
                                          )
                 , label = factor(label, levels = c("Pre-Covid", "Covid", "Post-Covid") #covid_period to factor
                                  )
                 , curfew_start = curfew_start_day
                 , curfew_end = curfew_end_day
                 , group = forcats::as_factor(group) #creates levels in the order in which they appear
                 ) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(y_max = max(actual, point_forecast_interruption1, point_forecast_interruption2, na.rm = TRUE)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(x_min = min(date)
                , x_max = max(date)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(int_obj = lubridate::interval(x_min, x_max)
                , x_mid = lubridate::as_date(lubridate::int_start(int_obj) + (lubridate::int_end(int_obj) - lubridate::int_start(int_obj))/2)
                ) %>%
  ggplot(aes(x=date, y = actual)) +
  geom_rect(aes(xmin=x_min, xmax=x_max, ymin=-Inf, ymax=Inf, fill = label), show.legend = FALSE) +
  scale_fill_manual("", values = c("Pre-Covid" = "ivory1", "Covid" = "thistle1", "Post-Covid" = "lightcyan1")) +
  geom_text(aes(x= x_mid, y= y_max, label = label), vjust = 0.5, hjust = 0.5, size = 2.8, color="gray45") +
  geom_line(aes(colour="Observed"),lty=1)+
  geom_line(aes(y = point_forecast_all,colour="Regression line"),lty=1)+
  geom_line(aes(y = point_forecast_interruption1,colour="Counterfactual(No Interruption)"),lty=2) +
  geom_line(aes(y = point_forecast_interruption2,colour="Counterfactual(Covid-19 Interruption)"),lty=2) +
  geom_vline(aes(xintercept = curfew_start, colour = "Curfew Start"), linetype = "dotdash") +
  geom_vline(aes(xintercept = curfew_end, colour = "Curfew End"), linetype = "dotdash") +
  scale_colour_manual("", values = c("Observed" = "gray45", "Regression line" = "red",
                                     "Counterfactual(No Interruption)" = "green2",
                                     "Counterfactual(Covid-19 Interruption)" = "blue",
                                     "Curfew Start" = "purple",
                                     "Curfew End" = "orange"
                                     )
                    ) +
  scale_y_continuous( n.breaks = 7, expand = expansion(mult = c(0.05,0.05)) ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01)) ) +
  labs(x= "Time(Weeks)", y = "Nutrient values per 100g/100ml") +
  guides(colour=guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
    axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
    axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
    axis.ticks.y = element_line(linewidth = 0.5, color="black"),
    axis.ticks.x = element_line(linewidth = 0.5, color="black"),
    axis.text.y = element_text(lineheight = 0.7, hjust = 0.5, size = 9),
    axis.text.x = element_text(angle = 90, lineheight = 0.7, vjust = 0.5, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.caption = element_text(angle = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold"),
    #strip.background.x = element_rect(fill = "grey80", linetype = 0),
    #strip.background.y = element_rect(fill = "grey80", linetype = 0), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +
  facet_wrap(~group, ncol = 2, scales = "free_y")

### Saving the ITS plots
ggsave(plot=arima_ITS_model_plot_weekly_mineral, height = 7.5, width = 13.5,
       filename = paste0("arima_ITS_plot_weekly_mineral",".png"),
       path = output_Dir, bg='white')


## Prediction plot
df_arima_prediction_model_weekly_mineral <- sapply(unique(df_analysis_ITS_weekly_mineral$new_label), function(x) {
  nn <- x
  
  df <- arima_model_all_weekly_mineral[[nn]][["predict_fit_df"]] %>%
    dplyr::select(-period) %>%
    dplyr::left_join(arima_model_interruption1_weekly_mineral[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date")) %>%
    dplyr::left_join(arima_model_interruption2_weekly_mineral[[nn]][["predict_fit_df"]] %>%
                       dplyr::select(-period)
                     , by = c("group", "date"))
  
}, simplify = FALSE
)


arima_prediction_model_plot_weekly_mineral <- data.table::rbindlist(df_arima_prediction_model_weekly_mineral) %>%
  dplyr::mutate(curfew_start = curfew_start_day
                , curfew_end = curfew_end_day
                , group = forcats::as_factor(group) #creates levels in the order in which they appear
                ) %>%
  ggplot(aes(x=date, y = actual)) +
  geom_line(aes(y = point_forecast_interruption1,colour="Predicted(No Interruption)"),lty=1) +
  geom_line(aes(y = point_forecast_interruption2,colour="Predicted(Covid-19 Interruption)"),lty=1) +
  geom_ribbon(aes(ymin = lo_95_interruption1, ymax = hi_95_interruption1, fill = "95% CI Predicted(No Interruption)"), alpha=0.2)+
  geom_ribbon(aes(ymin = lo_95_interruption2, ymax = hi_95_interruption2, fill = "95% CI Predicted(Covid-19 Interruption)"), alpha=0.2)+
  geom_line(aes(color="Observed"),lty=1)+
  geom_vline(aes(xintercept = curfew_start, colour = "Curfew Start"), linetype = "dotdash") +
  geom_vline(aes(xintercept = curfew_end, colour = "Curfew End"), linetype = "dotdash") +
  scale_colour_manual("", values = c("Observed" = "gray45",
                                     "Predicted(No Interruption)" = "green2",
                                     "Predicted(Covid-19 Interruption)" = "blue",
                                     "Curfew Start" = "purple",
                                     "Curfew End" = "orange"
                                     )
                    ) +
  scale_fill_manual("", values = c("95% CI Predicted(No Interruption)" = "green2", "95% CI Predicted(Covid-19 Interruption)" = "blue")) +
  scale_y_continuous( n.breaks = 7, expand = expansion(mult = c(0.05,0.05)) ) +
  scale_x_date( date_breaks = "3 months", date_labels = "%y-%m", expand = expansion(mult = c(0.01,0.01)) ) +
  labs(x= "Time(Weeks)", y = "Nutrient values per 100g/100ml") +
  guides(colour=guide_legend(nrow = 1)) +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
    axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
    axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
    axis.ticks.y = element_line(linewidth = 0.5, color="black"),
    axis.ticks.x = element_line(linewidth = 0.5, color="black"),
    axis.text.y = element_text(lineheight = 0.7, hjust = 0.5, size = 9),
    axis.text.x = element_text(angle = 90, lineheight = 0.7, vjust = 0.5, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.caption = element_text(angle = 0, size = 10, face = "italic"),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9, face = "bold"),
    #strip.background.x = element_rect(fill = "grey80", linetype = 0),
    #strip.background.y = element_rect(fill = "grey80", linetype = 0), 
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +
  facet_wrap(~group, ncol = 2, scales = "free_y")

### Saving the Prediction plots
ggsave(plot=arima_prediction_model_plot_weekly_mineral, height = 7.5, width = 13.5,
       filename = paste0("arima_prediction_plot_weekly_mineral",".png"),
       path = output_Dir, bg='white')


