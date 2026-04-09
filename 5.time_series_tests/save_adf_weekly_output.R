library(dplyr)
library(writexl)
library(data.table)

working_directory

## Saving Augmented Dickey–Fuller (ADF) t-statistic test for unit root Output

writexl::write_xlsx(dplyr::bind_rows(data.table::rbindlist(adf_ITS_weekly_nova)
                                     , data.table::rbindlist(adf_ITS_weekly_food_group)
                                     , data.table::rbindlist(adf_ITS_weekly_proximate)
                                     , data.table::rbindlist(adf_ITS_weekly_mineral)
                                     , data.table::rbindlist(adf_ITS_weekly_vitamin)
                                     ),
                    path = base::file.path(output_Dir, "adf_weekly.xlsx" )
                    )
