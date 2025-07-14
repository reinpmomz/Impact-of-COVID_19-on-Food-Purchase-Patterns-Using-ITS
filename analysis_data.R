library(dplyr)

working_directory

## dataset with variables for descriptive and inferential statistics

df_analysis <- df_clean_c_d %>%
  dplyr::filter(!food_group %in% unique(drop_selected_levels_df$level[drop_selected_levels_df$variable == "food_group"])
                ) %>%
  dplyr::select(-c(id, county, gender, description, price, quantity, total, trnref, paymentmode, branch, transaction_id,
                   dob_new, branch_name, county_name, sub_county_name, month_date, week_date, total_new, quantity_new,
                   price_new, customer_type, item_type, class_name, subclass_name, standard_uom, energy_k_j, ash_g, 
                   vit_a_rae_mcg, retinol_mcg, b_carotene_equivalent_mcg, food_folate_mcg
                   ), 
                -any_of(c("supermarket_name.x", "supermarket_name.y", "supermarket_name"))
                ) 


