library(dplyr)
library(tidyr)
library(writexl)
library(data.table)

working_directory

## Saving gls model weekly Output

### Nova
gls_model_interruption1_weekly_nova_model_metrics <- sapply(names(gls_model_interruption1_weekly_nova), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_nova[[nn]]$model_metrics
  
}, simplify = FALSE
)


gls_model_interruption1_weekly_nova_coefficients <- sapply(names(gls_model_interruption1_weekly_nova), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_nova[[nn]]$coefficients
  
}, simplify = FALSE
)

### Food group
gls_model_interruption1_weekly_food_group_model_metrics <- sapply(names(gls_model_interruption1_weekly_food_group), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_food_group[[nn]]$model_metrics
  
}, simplify = FALSE
)

gls_model_interruption1_weekly_food_group_coefficients <- sapply(names(gls_model_interruption1_weekly_food_group), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_food_group[[nn]]$coefficients
  
}, simplify = FALSE
)

### Proximate
gls_model_interruption1_weekly_proximate_model_metrics <- sapply(names(gls_model_interruption1_weekly_proximate), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_proximate[[nn]]$model_metrics
  
}, simplify = FALSE
)

gls_model_interruption1_weekly_proximate_coefficients <- sapply(names(gls_model_interruption1_weekly_proximate), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_proximate[[nn]]$coefficients
  
}, simplify = FALSE
)

### Mineral
gls_model_interruption1_weekly_mineral_model_metrics <- sapply(names(gls_model_interruption1_weekly_mineral), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_mineral[[nn]]$model_metrics
  
}, simplify = FALSE
)

gls_model_interruption1_weekly_mineral_coefficients <- sapply(names(gls_model_interruption1_weekly_mineral), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_mineral[[nn]]$coefficients
  
}, simplify = FALSE
)

### Vitamin
gls_model_interruption1_weekly_vitamin_model_metrics <- sapply(names(gls_model_interruption1_weekly_vitamin), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_vitamin[[nn]]$model_metrics
  
}, simplify = FALSE
)

gls_model_interruption1_weekly_vitamin_coefficients <- sapply(names(gls_model_interruption1_weekly_vitamin), function(x) {
  
  nn <- x
  
  metrics <- gls_model_interruption1_weekly_vitamin[[nn]]$coefficients
  
}, simplify = FALSE
)

writexl::write_xlsx(list(model_metrics = dplyr::bind_rows(data.table::rbindlist(gls_model_interruption1_weekly_nova_model_metrics)
                                                          , data.table::rbindlist(gls_model_interruption1_weekly_food_group_model_metrics)
                                                          , data.table::rbindlist(gls_model_interruption1_weekly_proximate_model_metrics)
                                                          , data.table::rbindlist(gls_model_interruption1_weekly_mineral_model_metrics)
                                                          , data.table::rbindlist(gls_model_interruption1_weekly_vitamin_model_metrics)
                                                          ),
                         coefficients_raw = dplyr::bind_rows(data.table::rbindlist(gls_model_interruption1_weekly_nova_coefficients)
                                                             , data.table::rbindlist(gls_model_interruption1_weekly_food_group_coefficients)
                                                             , data.table::rbindlist(gls_model_interruption1_weekly_proximate_coefficients)
                                                             , data.table::rbindlist(gls_model_interruption1_weekly_mineral_coefficients)
                                                             , data.table::rbindlist(gls_model_interruption1_weekly_vitamin_coefficients)
                                                             ),
                         coefficients_final = dplyr::bind_rows(data.table::rbindlist(gls_model_interruption1_weekly_nova_coefficients)
                                                               , data.table::rbindlist(gls_model_interruption1_weekly_food_group_coefficients)
                                                               , data.table::rbindlist(gls_model_interruption1_weekly_proximate_coefficients)
                                                               , data.table::rbindlist(gls_model_interruption1_weekly_mineral_coefficients)
                                                               , data.table::rbindlist(gls_model_interruption1_weekly_vitamin_coefficients)
                                                               ) %>%
                           dplyr::select(any_of(c("item", "group", "term", "Estimate", "Pr(>|z|)", "Pr(>|t|)", "2.5 %", "97.5 %"))
                                         ) %>%
                           dplyr::mutate(across(any_of(c("Pr(>|z|)", "Pr(>|t|)")), ~round(.x, 3))
                                         , across(any_of(c("Pr(>|z|)", "Pr(>|t|)")), ~ifelse(.x < 0.001, "<0.001", .x))
                                         , across(c("Estimate", "2.5 %", "97.5 %"), ~ifelse(item %in% c("nova", "food group"), .x*100, .x))
                                         , across(c("Estimate", "2.5 %", "97.5 %"), ~round(.x, 4))
                                         , across(c("Estimate", "2.5 %", "97.5 %"), ~format(.x, ,scientific = FALSE, trim = TRUE))
                                         , estimate_ci = paste0(Estimate, " (", `2.5 %`, ", ",`97.5 %`, ")")
                                         ) %>%
                           dplyr::select(any_of(c("item", "group", "term", "estimate_ci", "Pr(>|z|)", "Pr(>|t|)"))
                                         ) %>%
                           tidyr::pivot_wider(id_cols = c(item, group),
                                              names_from = term,
                                              values_from = any_of(c("estimate_ci", "Pr(>|z|)", "Pr(>|t|)")),
                                              names_vary = "slowest"
                                              )
                         ),
                    path = base::file.path(output_Dir, "gls_model_interruption1_weekly_output.xlsx" )
                    )
