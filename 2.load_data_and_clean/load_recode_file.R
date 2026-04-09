library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(tibble)

working_directory

## Reading the recode file sheet
recode_file <- read_excel_allsheets("./2.load_data_and_clean/supermarket_c_d_recode_file.xlsx")

branches_vars_df <- recode_file[["branches"]] #df for branches

rename_vars_df <- recode_file[["rename_vars"]] #df for renaming variable labels

covid_interruption_df <- recode_file[["covid_interruption"]] #df for covid-19 curfew start and end date

nutrient_composition_df <- recode_file[["food_nutrient_composition"]] #df for food nutrient composition

drop_selected_levels_df <- recode_file[["drop_selected_levels"]] #df for dropping selected variable levels

## Creating a named vector to quickly assign the new variable labels

new_labels <- rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  tidyr::drop_na(new_variable) %>%
  tibble::deframe()

