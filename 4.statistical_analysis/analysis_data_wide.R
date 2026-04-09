library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(fastDummies)

working_directory

### wide data of nova and food groups

df_analysis_wide <- df_clean_c_d %>%
  dplyr::select(-c(id, county, gender, description, price, quantity, total, trnref, paymentmode, branch, transaction_id,
                   dob_new, branch_name, county_name, sub_county_name, month_date, week_date, total_new, quantity_new,
                   price_new, customer_type, item_type, class_name, subclass_name, standard_uom, energy_k_j, ash_g, 
                   vit_a_rae_mcg, retinol_mcg, b_carotene_equivalent_mcg, food_folate_mcg
                   ), 
                -any_of(c("supermarket_name.x", "supermarket_name.y", "supermarket_name"))
                ) %>%
  dplyr::mutate(row_number = 1:n()
                ) %>%
  fastDummies::dummy_cols(select_columns = c("food_group", "nova")
                          ,ignore_na = TRUE #ignores any NA values in the column
                          , remove_selected_columns = FALSE #removes the columns used to generate the dummy columns.
                          , omit_colname_prefix = FALSE
                          ) %>%
  dplyr::mutate(across(starts_with(c("food_group_", "nova_")), ~if_else(.x == 1, "Yes", "No"))
                ) %>%
  dplyr::select(-row_number)

