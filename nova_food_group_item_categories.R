library(dplyr)
library(stringr)
library(data.table)

working_directory

## Nova/Food group item categories

nova_subclass_name_categories <- sapply(levels(factor(unique_items_vars_df$nova[!is.na(unique_items_vars_df$nova)])), function(x) {
  nn <- x
  df <- unique_items_vars_df %>%
    dplyr::filter(nova == nn) %>%
    dplyr::select(subclass_name) %>%
    dplyr::mutate(subclass_name = stringr::str_to_title(subclass_name)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(subclass_name) %>%
    dplyr::pull()
  
  out <- data.frame(nova = nn, 
                    subclass_name = paste(df, collapse = ", ")
                    )
  
}, simplify = FALSE
)


food_group_subclass_name_categories <- sapply(levels(factor(unique_items_vars_df$food_group[!is.na(unique_items_vars_df$food_group)])), function(x) {
  nn <- x
  df <- unique_items_vars_df %>%
    dplyr::filter(food_group == nn) %>%
    dplyr::select(subclass_name) %>%
    dplyr::mutate(subclass_name = stringr::str_to_title(subclass_name)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(subclass_name) %>%
    dplyr::pull()
  
  out <- data.frame(food_group = nn, 
                    subclass_name = paste(df, collapse = ", ")
                    )
  
}, simplify = FALSE
)

