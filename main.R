######################################################################
### Restart R
#.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T) #No subdirectories

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("requirements.R")

### helper/customized functions
source("helperfuns_read_excel_sheets.R")
source("helperfuns_gt_summary_themes.R")
source("helperfuns_table_summary_categorical.R")
source("helperfuns_table_summary_continous.R")
source("helperfuns_effect_size.R")
source("helperfuns_ggplot_themes.R")
source("helperfuns_grouped_line_plots.R")

### Load recode file
source("load_recode_file.R")

######################################################################
## Loading and Cleaning Data

### Load supermarket C, D Rdata file
source("load_data_local.R")

### Data cleaning
source("cleaning.R")

source("sample_supermarket_C_D.R")

### Create tables in database schema in Postgres
#source("create_tables_local_database.R")

### insert raw data in created tables
#source("insert_data_tables_local_database.R")

######################################################################
## Item categories
### nova/food group item categories
source("nova_food_group_item_categories.R")

### save output
source("save_nova_food_group_item_output.R")

## Plots
### Time series plots - proportion
source("nova_trend_year_prop_plots.R")
source("food_group_trend_year_prop_plots.R")
source("proximate_trend_year_plots.R")
source("mineral_trend_year_plots.R")
source("vitamin_trend_year_plots.R")

######################################################################
## Analysis - descriptive and inferential statistics

### Select variables for descriptive and inferential statistics
source("analysis_data_wide.R")

### check normality of numerical variables
source("check_normality_distribution.R")

### Descriptive and Inferential stats
source("descriptive_inferential_stats.R")

### Effect Size stats
source("effect_size_stats.R")

### Save stats output
source("save_descriptive_inferential_output.R")
source("save_effect_size_output.R")

######################################################################
## Statistical modelling - Weekly Interrupted Time series

### Select variables for ITS analysis
source("analysis_data.R") 

### Uncontrolled Interuppted Time series, two interventions

#### Pre-process data
source("analysis_data_ITS_weekly.R")

#### Augmented Dickey–Fuller (ADF) t-statistic test for unit root
source("adf_ITS_weekly_nova.R")
source("adf_ITS_weekly_food_group.R")
source("adf_ITS_weekly_proximate.R")
source("adf_ITS_weekly_mineral.R")
source("adf_ITS_weekly_vitamin.R")

source("save_adf_weekly_output.R")

#### Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity
source("kpss_ITS_weekly_nova.R")
source("kpss_ITS_weekly_food_group.R")
source("kpss_ITS_weekly_proximate.R")
source("kpss_ITS_weekly_mineral.R")
source("kpss_ITS_weekly_vitamin.R")

source("save_kpss_weekly_output.R")

#### Check autocorrelation (Serial correlation)
#### time series is stationary (constant in mean and variance and not dependent on time)
source("acf_ITS_weekly_nova.R")
source("acf_ITS_weekly_food_group.R")
source("acf_ITS_weekly_proximate.R")
source("acf_ITS_weekly_mineral.R")
source("acf_ITS_weekly_vitamin.R")

num_samples <- 100 #for prediction uncertainties

#### ARIMA Model
source("arima_model_weekly_nova.R")
source("arima_model_weekly_food_group.R")
source("arima_model_weekly_proximate.R")
source("arima_model_weekly_mineral.R")
source("arima_model_weekly_vitamin.R")

#### Saving ARIMA outputs
source("save_arima_model_all_weekly_output.R")
source("save_arima_model_interruption1_weekly_output.R")
source("save_arima_model_interruption2_weekly_output.R")

#### Generalized least squares Model
source("gls_model_weekly_nova.R")
source("gls_model_weekly_food_group.R")
source("gls_model_weekly_proximate.R")
source("gls_model_weekly_mineral.R")
source("gls_model_weekly_vitamin.R")

#### Saving gls outputs
source("save_gls_model_all_weekly_output.R")
source("save_gls_model_interruption1_weekly_output.R")
source("save_gls_model_interruption2_weekly_output.R")

#### Sensitivity analysis
source("sensitivity_gls_arima_model_all.R")
source("sensitivity_gls_arima_model_interruption1.R")
source("sensitivity_gls_arima_model_interruption2.R")

######################################################################
## Save workspace at the end without working directory path
save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_data", "data_Dir", 
                                                              "subDir_output", "output_Dir", "Rdata_files", 
                                                              "df_clean_supermarket_c", "df_clean_supermarket_d",
                                                              "df_analysis", "df_analysis_wide", "df_clean_c_d"
                                                              )],
     file = "supermarket_c_d_ITS.RData",
     envir = .GlobalEnv #parent.frame()
     )

######################################################################

## Run all files in Rstudio
source("main.R")

######################################################################

