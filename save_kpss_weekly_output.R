library(dplyr)
library(writexl)
library(data.table)

working_directory

## Saving Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity Output

writexl::write_xlsx(dplyr::bind_rows(data.table::rbindlist(kpss_ITS_weekly_nova)
                                     , data.table::rbindlist(kpss_ITS_weekly_food_group)
                                     , data.table::rbindlist(kpss_ITS_weekly_proximate)
                                     , data.table::rbindlist(kpss_ITS_weekly_mineral)
                                     , data.table::rbindlist(kpss_ITS_weekly_vitamin)
                                     ),
                    path = base::file.path(output_Dir, "kpss_weekly.xlsx" )
                    )
