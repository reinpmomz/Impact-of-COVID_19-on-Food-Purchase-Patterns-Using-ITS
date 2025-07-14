library(dplyr)
library(labelled)
library(tibble)
library(writexl)

working_directory

numerical_nutrient_vars <- c("energy_kcal", "water_g", "protein_g", "fat_g", "carbohydrate_available_g", "fibre_g",
                             "cholesterol_chole_mg", "calcium_ca_mg", "iron_fe_mg", "magnesium_mg_mg", "phosphorus_p_mg",
                             "potassium_k_mg", "sodium_na_mg", "zinc_zn_mg", "selenium_se_mcg", "vit_a_re_mcg",
                             "thiamin_mg", "riboflavin_mg", "niacin_mg", "dietary_folate_eq_mcg", "vit_b12_mcg", "vit_c_mg"
                             )

if (length(numerical_nutrient_vars)>0) {
  
  normality_test <- sapply(numerical_nutrient_vars, function(x) {
    nn <- x
    df_new <- df_analysis_wide %>%
      dplyr::select(all_of(nn)) %>%
      tidyr::drop_na(any_of(nn))
    
    label <- labelled::var_label(df_new[[nn]])

# Normality of Data - Normality test
## There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test.

## Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. 
## It is based on the correlation between the data and the corresponding normal scores.
## Shapiro-Wilk’s test has a maximum of 5000. Thus we will use K-S in this case

    test <- stats::ks.test(df_new[[nn]], y='pnorm')
    
    test_df <- tibble::tibble(variable = nn
                              , method = test[["method"]]
                              , exact = test[["exact"]]
                              , alternative = test[["alternative"]]
                              , statistic_D = test[["statistic"]]
                              , p_value = test[["p.value"]]
                              ) %>%
      dplyr::mutate(result = if_else(p_value < 0.05, "significant difference from normal distribution",
                                       "no significant difference from normal distribution")
                    )

  }, simplify = FALSE
  )
  
  writexl::write_xlsx(list(Normality = dplyr::bind_rows(normality_test)
                           ),
                      path = base::file.path(output_Dir, "normality_test.xlsx" )
                      )

} else {
  print(paste0("No normality checks done"))
}


