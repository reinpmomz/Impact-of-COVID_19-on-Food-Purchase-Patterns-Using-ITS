library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(labelled)

working_directory

## Extract curfew start and end days
curfew_start_day <- base::as.Date(covid_interruption_df$curfew_start)
curfew_end_day <- base::as.Date(covid_interruption_df$curfew_end)

## Clean food composition df
nutrient_composition_df <- (nutrient_composition_df %>%
                              janitor::clean_names() %>%
                              dplyr::mutate(across(where(is.character) & !c(nutrient_classification_document, food_group,
                                                                         code, food_name, edible_conversion_factor),
                                                   ~readr::parse_number(.x)
                                                   )
                                            , across(!c(nutrient_classification_document, food_group,
                                                        code, food_name, edible_conversion_factor),
                                                     ~if_else(.x == 0, NA, .x)
                                                     )
                                            )
                            )

## unique items data frame joined with food composition data
unique_items_vars_df <- recode_file[["unique_items"]] %>% 
  janitor::clean_names() %>%
  dplyr::distinct(description, .keep_all = TRUE) %>%
  dplyr::left_join(nutrient_composition_df %>%
                     dplyr::select(-c(food_group, food_name, edible_conversion_factor)
                                   ),
                   by = c("nutrient_classification_document", "code")
                   ) %>%
  dplyr::select(-c(nutrient_classification_document, code)
                )

df_clean_c_d <- df_raw_merge_c_d %>%
  dplyr::mutate(price = if_else(price == 0, NA, price) #Replacing price 0 with NA
                , sdatetime = base::as.Date(sdatetime) #format doesn't store any time information
                , month_date = lubridate::floor_date(sdatetime, unit = "month")
                , week_date = lubridate::floor_date(sdatetime, unit = "week", week_start = 1) #week starts monday
                , week_number = lubridate::week(sdatetime) #complete 7day periods occurred between the date and Jan 1st, plus 1
                , day_year = lubridate::yday(sdatetime) #day of year
                ) %>%
  dplyr::group_by(description, month_date) %>%
  dplyr::mutate(price = if_else(is.na(price), round(mean(price, na.rm= TRUE),2),
                                price) #Replacing where price is NA with mean for items
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( total_new = price*quantity
                 , quantity_new = if_else((quantity %% 1) > 0, 1, quantity) #modulo operator with decimal numbers
                 , price_new = total_new/quantity_new
                 , customer_type = ifelse(is.na(id), "No-Loyalty card", "Loyalty card")
                 , month_name = lubridate::month(sdatetime,  label = TRUE, abbr = TRUE)
                 , year = lubridate::year(sdatetime)
                 , quarter_date = as.factor(paste0("Q",lubridate::quarter(sdatetime, type = "quarter")))
                 , covid_period = if_else(sdatetime < curfew_start_day, "Pre-Covid",
                                          if_else(sdatetime < curfew_end_day, "Covid",
                                                  "Post-Covid"
                                                  )
                                          )
                 , covid_period = factor(covid_period, levels = c("Pre-Covid", "Covid", "Post-Covid"
                                                                  ) #covid_period to factor
                                         )
                 ) %>%
  dplyr::left_join(unique_items_vars_df
                   , by = c("description")
                   ) %>%
  dplyr::filter(quantity > 0, item_type == "Food Item") %>%
  labelled::set_variable_labels(!!!new_labels[names(new_labels) %in% names(.)] #labeling variables from named vector
                                )

