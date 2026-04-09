library(dplyr)

working_directory

## Load Rdata from subdirectory data

invisible(
  lapply(X=list.files(path = data_Dir, pattern = "*.RData", full.names = T),load,.GlobalEnv)
  )

df_raw_merge_c_d <- dplyr::bind_rows(df_clean_supermarket_c, df_clean_supermarket_d)


