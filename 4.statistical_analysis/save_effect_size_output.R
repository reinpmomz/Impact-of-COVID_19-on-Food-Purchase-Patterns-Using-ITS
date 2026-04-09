library(dplyr)
library(writexl)

working_directory

## Saving effect size stats Output

if (length(inferential_vars)>0) {
  
  writexl::write_xlsx(effect_size_stats,
                      path = base::file.path(output_Dir, "effect_size.xlsx" )
                      )

} else {
  print(paste0("No effect size analysis done"))
}


