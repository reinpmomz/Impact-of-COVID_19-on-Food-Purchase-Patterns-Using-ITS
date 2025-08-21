library(dplyr)
library(forcats)
library(data.table)
library(ggplot2)

working_directory

## Sensitivity Analysis - Comparing coefficients and p-values

sensitivity_gls_arima_model_all_weekly <- dplyr::bind_rows(data.table::rbindlist(gls_model_all_weekly_nova_coefficients)
                                                           , data.table::rbindlist(gls_model_all_weekly_food_group_coefficients)
                                                           , data.table::rbindlist(gls_model_all_weekly_proximate_coefficients)
                                                           , data.table::rbindlist(gls_model_all_weekly_mineral_coefficients)
                                                           , data.table::rbindlist(gls_model_all_weekly_vitamin_coefficients)
                                                           ) %>%
  dplyr::select(any_of(c("item", "group", "term", "Estimate", "Pr(>|z|)", "Pr(>|t|)"))
                ) %>%
  dplyr::mutate(across(any_of(c("Pr(>|z|)", "Pr(>|t|)")), ~round(.x, 3))
                , across(c("Estimate"), ~ifelse(item %in% c("nova", "food group"), .x*100, .x))
                , across(c("Estimate"), ~round(.x, 5))
                #, across(c("Estimate"), ~format(.x, ,scientific = FALSE, trim = TRUE))
                , term = ifelse(term == "(Intercept)", "intercept",
                                ifelse(term == "weekly_time", "xreg1", 
                                       ifelse(term == "intervention1", "xreg2", 
                                              ifelse(term == "post_intervention1_time", "xreg3", 
                                                     ifelse(term == "intervention2", "xreg4", 
                                                            ifelse(term == "post_intervention2_time", "xreg5", term
                                                                   ))))))
                ) %>%
  dplyr::rename(any_of(c(estimate_gls = "Estimate", pvalue_gls = "Pr(>|z|)", pvalue_gls = "Pr(>|t|)"))
                ) %>%
  dplyr::inner_join(dplyr::bind_rows(data.table::rbindlist(arima_model_all_weekly_nova_coefficients)
                                     , data.table::rbindlist(arima_model_all_weekly_food_group_coefficients)
                                     , data.table::rbindlist(arima_model_all_weekly_proximate_coefficients)
                                     , data.table::rbindlist(arima_model_all_weekly_mineral_coefficients)
                                     , data.table::rbindlist(arima_model_all_weekly_vitamin_coefficients)
                                     ) %>%
                      dplyr::filter(term %in% c("intercept", "xreg", "xreg1", "xreg2", "xreg3", "xreg4", "xreg5")) %>%
                      dplyr::select(any_of(c("item", "group", "term", "Estimate", "Pr(>|z|)", "Pr(>|t|)"))
                                    ) %>%
                      dplyr::mutate(across(any_of(c("Pr(>|z|)", "Pr(>|t|)")), ~round(.x, 3))
                                    , across(c("Estimate"), ~ifelse(item %in% c("nova", "food group"), .x*100, .x))
                                    , across(c("Estimate"), ~round(.x, 5))
                                    #, across(c("Estimate"), ~format(.x, ,scientific = FALSE, trim = TRUE))
                                    ) %>%
                      dplyr::rename(any_of(c(estimate_arima = "Estimate", pvalue_arima = "Pr(>|z|)", pvalue_arima = "Pr(>|t|)"))
                                    ),
                    by = c("item", "group", "term")
                    ) %>%
  dplyr::mutate(term = ifelse(term == "intercept", paste0("\U03B2", "\U2080", " (Intercept)"),
                              ifelse(term == "xreg1", paste0("\U03B2", "\U2081", " (Pre-Covid)"), 
                                     ifelse(term == "xreg2", paste0("\U03B2", "\U2082", " (Start of Lockdown)"),
                                            ifelse(term == "xreg3", paste0("\U03B2", "\U2083", " (Covid Period)"),
                                                   ifelse(term == "xreg4", paste0("\U03B2", "\U2084", " (End of Lockdown)"),
                                                          ifelse(term == "xreg5", paste0("\U03B2", "\U2085", " (Post-Covid)"),
                                                                 term
                                                          ))))))
                , across(c("item", "group", "term"), ~forcats::as_factor(.x))
                , cutt_off_pvalue_gls = ifelse(pvalue_gls <0.05, "TRUE", "FALSE")
                , cutt_off_pvalue_arima = ifelse(pvalue_arima <0.05, "TRUE", "FALSE")
                , same_pvalue = ifelse(cutt_off_pvalue_gls == cutt_off_pvalue_arima, "TRUE", "FALSE")
                , same_magnitude_coeff = ifelse(sign(estimate_gls) == sign(estimate_arima), "TRUE", "FALSE")
                , same_magnitude_sign = ifelse(same_magnitude_coeff == "TRUE", sign(estimate_gls), NA)
                , same_magnitude_sign = if_else(same_magnitude_sign < 0, "-", "+")
                , sensitivity = ifelse(same_magnitude_coeff == "TRUE" & same_pvalue == "TRUE" & pvalue_gls < 0.05 
                                       & pvalue_arima <0.05, "Significant Stable Coefficient",
                                ifelse(same_magnitude_coeff == "TRUE" & same_pvalue == "TRUE" & pvalue_gls > 0.05
                                       & pvalue_arima >0.05, "Non-Significant Stable Coefficient",
                                ifelse(same_magnitude_coeff == "TRUE" & same_pvalue == "FALSE" & cutt_off_pvalue_gls == "TRUE"
                                       & cutt_off_pvalue_arima == "FALSE", "Stable Coefficient Significant in GLS only",
                                ifelse(same_magnitude_coeff == "TRUE" & same_pvalue == "FALSE" & cutt_off_pvalue_gls == "FALSE"
                                       & cutt_off_pvalue_arima == "TRUE", "Stable Coefficient Significant in ARIMA only",
                                ifelse(same_magnitude_coeff == "FALSE" & same_pvalue == "TRUE" & pvalue_gls < 0.05 
                                       & pvalue_arima <0.05, "Significant Unstable Coefficient",
                                ifelse(same_magnitude_coeff == "FALSE" & same_pvalue == "TRUE" & pvalue_gls > 0.05
                                       & pvalue_arima >0.05, "Non-Significant Unstable Coefficient",
                                ifelse(same_magnitude_coeff == "FALSE" & same_pvalue == "FALSE" & cutt_off_pvalue_gls == "TRUE"
                                       & cutt_off_pvalue_arima == "FALSE", "Unstable Coefficient Significant in GLS only",
                                ifelse(same_magnitude_coeff == "FALSE" & same_pvalue == "FALSE" & cutt_off_pvalue_gls == "FALSE"
                                       & cutt_off_pvalue_arima == "TRUE", "Unstable Coefficient Significant in ARIMA only", NA ))))))))
                ) %>%
  dplyr::select(any_of(c("item", "group", "term", "same_magnitude_sign", "sensitivity"))
                ) %>%
  ggplot(aes(x= term, y=forcats::fct_rev(group), fill=sensitivity)) + 
  geom_tile(colour = "grey50") +
  geom_text(aes(label = same_magnitude_sign), color = "grey25", size = 4) +
  scale_fill_manual("", values = c("Significant Stable Coefficient" = "#00BF7D", "Non-Significant Stable Coefficient" = "grey",
                                   "Stable Coefficient Significant in GLS only" = "#E76BF3",
                                   "Stable Coefficient Significant in ARIMA only" = "#00B0F6",
                                   "Non-Significant Unstable Coefficient" = "#A3A500")
                    ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand=c(0,0)
                   ,labels = function(x) stringr::str_wrap(x, width = 80)
                   ) +
  labs(fill = "", x=NULL, y=NULL) +
  guides(fill=guide_legend(nrow = 2)) +
  theme_bw(base_size = 10) + 
  theme(panel.grid = element_blank()
        , legend.position="bottom"
        , legend.text = element_text(size = 9)
        , plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
        , axis.title.x = element_blank()
        , axis.title.y = element_blank()
        , axis.text.y = element_text(size = 9, angle = 0, lineheight = 0.7)
        , axis.text.x = element_text(size = 11, angle = 0, lineheight = 0.7, family = "serif") #sans, mono
        , strip.background = element_blank()
        , strip.text.x = element_text(size = 8
                                    , colour = "black"
                                    , face = "bold"
                                    )
        )

print(sensitivity_gls_arima_model_all_weekly)

### Saving the sensitivity plot
ggsave(plot=sensitivity_gls_arima_model_all_weekly, height = 7, width = 11,
       filename = paste0("sensitivity_gls_arima_model_all_weekly",".png"),
       path = output_Dir, bg='white')

#scales::show_col(scales::hue_pal(direction=-1)(4))