# Impact-of-COVID_19-on-Food-Purchase-Patterns-Using-ITS
Utilizing Interrupted Time Series analysis to examine changes in food purchasing and nutritional composition before, during, and after the COVID-19 pandemic.

## Background

After the emergence of COVID-19 caused by the novel severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) in December 2019, the World Health Organization (WHO)
officially declared it as a global pandemic on 11th March 2020. Social, public, and individual life was severely disrupted by the COVID-19 pandemic. Kenya, like many other countries, was significantly affected by a series of COVID-19 waves and circulation of SARS-CoV-2 variants.

As part of mitigation measures to prevent the spread of COVID-19, the Kenyan Government implemented a nationwide lockdown on 27th March 2020, which included the closure of all but ‘essential’ businesses, including full-service restaurants and other out-of-home (OOH) food establishments. The lockdown adversely affected the food supply chain, transport, food security, healthcare services, employment, social interactions, and income levels. Additionally, there was a significant impact on health and consumer behaviors, including changes in daily routines, sleep, exercise, sedentary behavior, diet, and food purchase patterns.

## Summary

The **"Analysis of supermarket grocery data for prediction of nutritional and health outcomes at the population level in Kenya"** project aims to examine changes in food purchase patterns and nutrient composition before, during, and after the COVID-19 pandemic. This will provide valuable insights for policymakers and healthcare experts to implement targeted policies that curb the consumption of unhealthy foods. Additionally, this study sheds light on how populations respond during and after global crises, which can shape future policies aimed at strengthening food security in Kenya and similar settings elsewhere.

## Setup

We are assuming you have `R Software` and `Rstudio IDE` installed. If not you can download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis is available on reasonable request from the [**Study PI - Agnes Kiragga**](mailto:akiragga@aphrc.org?subject=[GitHub]%20Source%20Han%20Sans).

- **Data used for analysis:** `clean_supermarket_c.RData` and `clean_supermarket_d.RData`

- Data anonymized at the supermarket and client level

- Transactional data with 11,229,879 records from January 2018 to December 2023 of purchases in 2 supermarket chains, each with 1 branch in Nairobi County.

- Transactional data further filtered by removing non-food items. The final dataset for analysis had 11,105,974 records.

- Food items were categorized into nineteen food groups, which were further classified into 4 groups according to the Nova food classification, and combined with food composition data.

## Tools/Materials

1. Nova food classification guides are in the _data_ sub-folder of this repository.

2. The `supermarket_c_d_recode_file.xlsx` file contains: 
    
    1. Metadata about supermarkets in the _branches_ sheet.
  
    2. Guide on official COVID-19 lockdown start and end dates by the Kenyan Government. 
   
    3. Guide to how food items were categorized in the _unique_items_ sheet.
    
    4. Data dictionary of the data _rename_vars_ sheet.
  
    5. Food composition data in the _food_nutrient_composition_ sheet.
  
    6. Food groups whose transactions were dropped from the ITS analysis in the _drop_selected_levels_ sheet.
   
## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Impact-of-COVID_19-on-Food-Purchase-Patterns-Using-ITS_ folder.

Open `RStudio` then set your working directory to the _Impact-of-COVID_19-on-Food-Purchase-Patterns-Using-ITS_ folder. 

- If you get data for this project, it is advisable to work on a `RStudio Server` setup and run individual files. To run individual files, open the `main.R` script, and run from the beginning.
- To run all files at once, copy the below code in RStudio and run

```
source("main.R")

```



