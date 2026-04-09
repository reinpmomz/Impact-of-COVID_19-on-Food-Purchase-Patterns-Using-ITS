library(dplyr)
library(writexl)

working_directory

## Nova/Food group item categories

writexl::write_xlsx(list(nova_subclass_name = data.table::rbindlist(nova_subclass_name_categories),
                         food_group_subclass_name = data.table::rbindlist(food_group_subclass_name_categories)
                         ),
                    path = base::file.path(output_Dir, "nova_food_group_subclass_name.xlsx" )
                    )



