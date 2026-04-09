library(dplyr)
library(haven)


working_directory

## saving sample of merged dataset
set.seed(231)

haven::write_dta(data= df_clean_c_d %>%
                   dplyr::select(-any_of(c("quantity_new", "price_new", "month_name", "age_group", "standard_uom", 
                                           "uom_criteria", "conversion", "quantity_uom", "price_uom", "class_name_uom"))
                                 ) %>%
                   dplyr::slice_sample(prop = 0.04,
                                       by = branch
                                       ),
                 version = 10, #Align with version supported by nestar publisher
                 path = base::file.path(output_Dir, "clean_analysis_supermarket_c_d_sample.dta")
                 )

