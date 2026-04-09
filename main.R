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
source("./1.setup/requirements.R")

### helper/customized functions
source("./1.setup/helperfuns_read_excel_sheets.R")
source("./1.setup/helperfuns_gt_summary_themes.R")
source("./1.setup/helperfuns_table_summary_categorical.R")
source("./1.setup/helperfuns_table_summary_continous.R")
source("./1.setup/helperfuns_effect_size.R")
source("./1.setup/helperfuns_ggplot_themes.R")
source("./1.setup/helperfuns_grouped_line_plots.R")

######################################################################
## Loading and Cleaning Data

### Load recode file
source("./2.load_data_and_clean/load_recode_file.R")

### Load supermarket C, D Rdata file
source("./2.load_data_and_clean/load_data_local.R")

### Data cleaning
source("./2.load_data_and_clean/cleaning.R")

source("./2.load_data_and_clean/sample_supermarket_C_D.R")

######################################################################
#Create connection to Database
## Connecting to Supermarket local Database - connects to public schema

con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                 dbname = 'supermarket', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 #password = rstudioapi::askForPassword("Database password")
                 password = Sys.getenv("postgres_password")
                 
                 )

print(con)


## Inserting data to specific schema and table in database
DBI::dbWriteTable(con
                  , name = Id(schema = "raw_clean_c_d", table = "clean_merge_c_d")
                  , value = df_clean_c_d %>% dplyr::select(id:covid_period)
                  , overwrite = TRUE
                  , row.names = FALSE
                  )

DBI::dbWriteTable(con
                  , name = Id(schema = "raw_clean_c_d", table = "link_file_nova_nutrient_c_d")
                  , value = df_clean_c_d %>% 
                    dplyr::select(description, item_type:cholesterol_chole_mg) %>%
                    dplyr::distinct(description, .keep_all = TRUE)
                  , overwrite = TRUE
                  , row.names = FALSE
                  )

## Disconnect database 
DBI::dbDisconnect(con)

######################################################################
## Item categories
### nova/food group item categories
source("./3.descriptive_trend_plots/nova_food_group_item_categories.R")

### save output
source("./3.descriptive_trend_plots/save_nova_food_group_item_output.R")

## Plots
### Time series plots - proportion
source("./3.descriptive_trend_plots/nova_trend_year_prop_plots.R")
source("./3.descriptive_trend_plots/food_group_trend_year_prop_plots.R")
source("./3.descriptive_trend_plots/proximate_trend_year_plots.R")
source("./3.descriptive_trend_plots/mineral_trend_year_plots.R")
source("./3.descriptive_trend_plots/vitamin_trend_year_plots.R")

######################################################################
## Analysis - descriptive and inferential statistics

### Select variables for descriptive and inferential statistics
source("./4.statistical_analysis/analysis_data_wide.R")

### check normality of numerical variables
source("./4.statistical_analysis/check_normality_distribution.R")

### Descriptive and Inferential stats
source("./4.statistical_analysis/descriptive_inferential_stats.R")

### Effect Size stats
source("./4.statistical_analysis/effect_size_stats.R")

### Save stats output
source("./4.statistical_analysis/save_descriptive_inferential_output.R")
source("./4.statistical_analysis/save_effect_size_output.R")

######################################################################
## Statistical modelling - Weekly Interrupted Time series

### Select variables for ITS analysis
source("./5.time_series_tests/analysis_data.R") 

### Uncontrolled Interuppted Time series, two interventions

#### Pre-process data
source("./5.time_series_tests/analysis_data_ITS_weekly.R")

#### Augmented Dickey–Fuller (ADF) t-statistic test for unit root
source("./5.time_series_tests/adf_ITS_weekly_nova.R")
source("./5.time_series_tests/adf_ITS_weekly_food_group.R")
source("./5.time_series_tests/adf_ITS_weekly_proximate.R")
source("./5.time_series_tests/adf_ITS_weekly_mineral.R")
source("./5.time_series_tests/adf_ITS_weekly_vitamin.R")

source("./5.time_series_tests/save_adf_weekly_output.R")

#### Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity
source("./5.time_series_tests/kpss_ITS_weekly_nova.R")
source("./5.time_series_tests/kpss_ITS_weekly_food_group.R")
source("./5.time_series_tests/kpss_ITS_weekly_proximate.R")
source("./5.time_series_tests/kpss_ITS_weekly_mineral.R")
source("./5.time_series_tests/kpss_ITS_weekly_vitamin.R")

source("./5.time_series_tests/save_kpss_weekly_output.R")

#### Check autocorrelation (Serial correlation)
#### time series is stationary (constant in mean and variance and not dependent on time)
source("./5.time_series_tests/acf_ITS_weekly_nova.R")
source("./5.time_series_tests/acf_ITS_weekly_food_group.R")
source("./5.time_series_tests/acf_ITS_weekly_proximate.R")
source("./5.time_series_tests/acf_ITS_weekly_mineral.R")
source("./5.time_series_tests/acf_ITS_weekly_vitamin.R")

######################################################################
## Interrupted Time series -  ARIMA Model

num_samples <- 100 #for prediction uncertainties

#### ARIMA Model
source("./6.ITS_model_arima/arima_model_weekly_nova.R")
source("./6.ITS_model_arima/arima_model_weekly_food_group.R")
source("./6.ITS_model_arima/arima_model_weekly_proximate.R")
source("./6.ITS_model_arima/arima_model_weekly_mineral.R")
source("./6.ITS_model_arima/arima_model_weekly_vitamin.R")

#### Saving ARIMA outputs
source("./6.ITS_model_arima/save_arima_model_all_weekly_output.R")
source("./6.ITS_model_arima/save_arima_model_interruption1_weekly_output.R")
source("./6.ITS_model_arima/save_arima_model_interruption2_weekly_output.R")

######################################################################
## Interrupted Time series -  GLS Model

#### Generalized least squares Model
source("./7.ITS_model_gls/gls_model_weekly_nova.R")
source("./7.ITS_model_gls/gls_model_weekly_food_group.R")
source("./7.ITS_model_gls/gls_model_weekly_proximate.R")
source("./7.ITS_model_gls/gls_model_weekly_mineral.R")
source("./7.ITS_model_gls/gls_model_weekly_vitamin.R")

#### Saving gls outputs
source("./7.ITS_model_gls/save_gls_model_all_weekly_output.R")
source("./7.ITS_model_gls/save_gls_model_interruption1_weekly_output.R")
source("./7.ITS_model_gls/save_gls_model_interruption2_weekly_output.R")

######################################################################
## Sensitivity Analysis
### GLS Model vs ARIMA Model

source("./8.sensitivity_analysis/sensitivity_gls_arima_model_all.R")
source("./8.sensitivity_analysis/sensitivity_gls_arima_model_interruption1.R")
source("./8.sensitivity_analysis/sensitivity_gls_arima_model_interruption2.R")

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

