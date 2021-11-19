# load packages to read, write and process data

install.packages("tidyverse")
# install.packages("sqldf")
# install.packages("plyr")

library(readr)
library(tidyverse)
library(knitr)
rm(list=ls())
library(purrr)

# library(dplyr)
# library(sqldf)
# library(plyr)


### HEALTHCARE AND HEALTH ECONOMIC DATA ###

# This set of data does NOT have ENTITIES 
Development_Health_Assistance_IHME <- read_csv("Working_Data/healthcare/Development Health Assistance - IHME.csv")
##View(Development_Health_Assistance_IHME)
Development_Health_Assistance_IHME_df <- data.frame(Development_Health_Assistance_IHME)

# Load data, create dataframes, keep data of most recent year, remove year
 
Attitudes_to_Vaccines_Wellcome_Trust_2019 <- read_csv("Working_Data/healthcare/Attitudes to Vaccines - Wellcome Trust (2019).csv")
##View(Attitudes_to_Vaccines_Wellcome_Trust_2019)
Attitudes_to_Vaccines_Wellcome_Trust_2019_df <- data.frame(Attitudes_to_Vaccines_Wellcome_Trust_2019)
Attitudes_to_Vaccines_Wellcome_Trust_2019_df1 <- Attitudes_to_Vaccines_Wellcome_Trust_2019_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

Clark_Flèche_Senik_Happiness_Inequality <- read_csv("Working_Data/healthcare/Clark, Flèche & Senik – Happiness Inequality.csv")
##View(Clark_Flèche_Senik_Happiness_Inequality)
Clark_Flèche_Senik_Happiness_Inequality_df <- data.frame(Clark_Flèche_Senik_Happiness_Inequality)
Clark_Flèche_Senik_Happiness_Inequality_df1 <- Clark_Flèche_Senik_Happiness_Inequality_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df1 <- left_join(Attitudes_to_Vaccines_Wellcome_Trust_2019_df1, Clark_Flèche_Senik_Happiness_Inequality_df1, by = "Entity")

Disability_Adjusted_Life_Years_WHO_2015_ <- read_csv("Working_Data/healthcare/Disability Adjusted Life Years - WHO (2015).csv")
##View(Disability_Adjusted_Life_Years_WHO_2015_)
Disability_Adjusted_Life_Years_WHO_2015_df <- data.frame(Disability_Adjusted_Life_Years_WHO_2015_)
Disability_Adjusted_Life_Years_WHO_2015_df1 <- Disability_Adjusted_Life_Years_WHO_2015_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df2 <- left_join(Disability_Adjusted_Life_Years_WHO_2015_df1,df1, by = "Entity")

Health_Coverage_ILO_2014_ <- read_csv("Working_Data/healthcare/Health Coverage – ILO (2014).csv")
##View(Health_Coverage_ILO_2014_)
Health_Coverage_ILO_2014_df <- data.frame(Health_Coverage_ILO_2014_)
Health_Coverage_ILO_2014_df1 <- Health_Coverage_ILO_2014_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df3 <- left_join(Health_Coverage_ILO_2014_df1, df2, by = "Entity")

Health_Expenditure_Tanzi_Schuknecht_2000_ <- read_csv("Working_Data/healthcare/Health Expenditure - Tanzi & Schuknecht (2000).csv")
##View(Health_Expenditure_Tanzi_Schuknecht_2000_)
Health_Expenditure_Tanzi_Schuknecht_2000_df <- data.frame(Health_Expenditure_Tanzi_Schuknecht_2000_)
Health_Expenditure_Tanzi_Schuknecht_2000_df1 <- Health_Expenditure_Tanzi_Schuknecht_2000_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df4 <- left_join(Health_Expenditure_Tanzi_Schuknecht_2000_df1, df3, by = "Entity")

Health_Expenditure_and_Financing_OECDstat_2017_ <- read_csv("Working_Data/healthcare/Health Expenditure and Financing - OECDstat (2017).csv")
##View(Health_Expenditure_and_Financing_OECDstat_2017_)
Health_Expenditure_and_Financing_OECDstat_2017_df <- data.frame(Health_Expenditure_and_Financing_OECDstat_2017_)
Health_Expenditure_and_Financing_OECDstat_2017_df1 <- Health_Expenditure_and_Financing_OECDstat_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df5 <- left_join(Health_Expenditure_and_Financing_OECDstat_2017_df1, df4, by = "Entity")

Health_expenditure_per_capita_World_Bank_WDI_2018_ <- read_csv("Working_Data/healthcare/Health expenditure per capita - World Bank WDI (2018).csv")
##View(Health_expenditure_per_capita_World_Bank_WDI_2018_)
Health_expenditure_per_capita_World_Bank_WDI_2018_df <- data.frame(Health_expenditure_per_capita_World_Bank_WDI_2018_)
Health_expenditure_per_capita_World_Bank_WDI_2018_df1 <- Health_expenditure_per_capita_World_Bank_WDI_2018_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df6 <- left_join(Health_expenditure_per_capita_World_Bank_WDI_2018_df1, df5, by = "Entity")

Health_expenditure_per_capita_PPP_World_Bank_2016_ <- read_csv("Working_Data/healthcare/Health expenditure per capita, PPP - World Bank (2016).csv")
#View(Health_expenditure_per_capita_PPP_World_Bank_2016_)
Health_expenditure_per_capita_PPP_World_Bank_2016_df <- data.frame(Health_expenditure_per_capita_PPP_World_Bank_2016_)
Health_expenditure_per_capita_PPP_World_Bank_2016_df1 <- Health_expenditure_per_capita_PPP_World_Bank_2016_df %>%
    group_by(Entity) %>%
    slice_max(Year) %>% 
    select(-Year)

df7 <- left_join(Health_expenditure_per_capita_PPP_World_Bank_2016_df1, df6, by = "Entity")

Healthcare_Access_and_Quality_Index_IHME_2017_ <- read_csv("Working_Data/healthcare/Healthcare Access and Quality Index – IHME (2017).csv")
#View(Healthcare_Access_and_Quality_Index_IHME_2017_)
Healthcare_Access_and_Quality_Index_IHME_2017_df <- data.frame(Healthcare_Access_and_Quality_Index_IHME_2017_)
Healthcare_Access_and_Quality_Index_IHME_2017_df1 <- Healthcare_Access_and_Quality_Index_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df8 <- left_join(Healthcare_Access_and_Quality_Index_IHME_2017_df1, df7, by = "Entity")

Healthcare_capacity_OECD_2020_ <- read_csv("Working_Data/healthcare/Healthcare capacity (OECD, 2020).csv")
#View(Healthcare_capacity_OECD_2020_)
Healthcare_capacity_OECD_2020_df <- data.frame(Healthcare_capacity_OECD_2020_)
Healthcare_capacity_OECD_2020_df1 <- Healthcare_capacity_OECD_2020_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df9 <- left_join(Healthcare_capacity_OECD_2020_df1, df8, by = "Entity")

Healthy_Life_Expectancy_IHME <- read_csv("Working_Data/healthcare/Healthy Life Expectancy - IHME.csv")
#View(Healthy_Life_Expectancy_IHME)
Healthy_Life_Expectancy_IHME_df <- data.frame(Healthy_Life_Expectancy_IHME)
Healthy_Life_Expectancy_IHME_df1 <- Healthy_Life_Expectancy_IHME_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df10 <- left_join(Healthy_Life_Expectancy_IHME_df1, df9, by = "Entity")

Long_run_series_of_health_expenditure_World_Bank_WDI_2017_ <- read_csv("Working_Data/healthcare/Long-run series of health expenditure - World Bank (WDI) (2017).csv")
#View(Long_run_series_of_health_expenditure_World_Bank_WDI_2017_)
Long_run_series_of_health_expenditure_World_Bank_WDI_2017_df <- data.frame(Long_run_series_of_health_expenditure_World_Bank_WDI_2017_)
Long_run_series_of_health_expenditure_World_Bank_WDI_2017_df1 <- Long_run_series_of_health_expenditure_World_Bank_WDI_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df11 <- left_join(Long_run_series_of_health_expenditure_World_Bank_WDI_2017_df1, df10, by = "Entity")

OECD_Social_Spending_Health <- read_csv("Working_Data/healthcare/OECD Social Spending- Health.csv")
#View(OECD_Social_Spending_Health)
OECD_Social_Spending_Health_df <- data.frame(OECD_Social_Spending_Health)
OECD_Social_Spending_Health_df1 <- OECD_Social_Spending_Health_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df12 <- left_join(OECD_Social_Spending_Health_df1, df11, by = "Entity")

OECD_Social_Spending_Old_Age <- read_csv("Working_Data/healthcare/OECD Social Spending- Old Age.csv")
#View(OECD_Social_Spending_Old_Age)
OECD_Social_Spending_Old_Age_df <- data.frame(OECD_Social_Spending_Old_Age)
OECD_Social_Spending_Old_Age_df1 <- OECD_Social_Spending_Old_Age_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df13 <- left_join(OECD_Social_Spending_Old_Age_df1, df12, by = "Entity")

Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure <- read_csv("Working_Data/healthcare/Out-of-pocket expenditure per capita on healthcare - WHO Global Health Expenditure.csv")
#View(Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure)
Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure_df <- data.frame(Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure)
Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure_df1 <- Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df14 <- left_join(Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure_df1, df13, by = "Entity")

Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo <- read_csv("Working_Data/healthcare/Percentage of persons without health insurance - Council of Economic Advisers and National Center fo.csv")
#View(Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo)
Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo_df <- data.frame(Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo)
Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo_df1 <- Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df15 <- left_join(Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo_df1, df14, by = "Entity")

Perceptions_of_spending_on_health_expenditure_IPSOS_2016_ <- read_csv("Working_Data/healthcare/Perceptions of spending on health expenditure - IPSOS (2016).csv")
#View(Perceptions_of_spending_on_health_expenditure_IPSOS_2016_)
Perceptions_of_spending_on_health_expenditure_IPSOS_2016_df <- data.frame(Perceptions_of_spending_on_health_expenditure_IPSOS_2016_)
Perceptions_of_spending_on_health_expenditure_IPSOS_2016_df1 <- Perceptions_of_spending_on_health_expenditure_IPSOS_2016_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df16 <- left_join(Perceptions_of_spending_on_health_expenditure_IPSOS_2016_df1, df15, by = "Entity")

Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates <- read_csv("Working_Data/healthcare/Public expenditure on health %GDP – OWID based on WHO and historical estimates.csv")
#View(Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates)
Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates_df <- data.frame(Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates)
Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates_df1 <- Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df17 <- left_join(Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates_df1, df16, by = "Entity")

Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD <- read_csv("Working_Data/healthcare/Total gross official disbursements for medical research and basic heath sectors - OECD.csv")
#View(Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD)
Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD_df <- data.frame(Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD)
Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD_df1 <- Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df18 <- left_join(Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD_df1, df17, by = "Entity")

Vaccine_Coverage_and_Disease_Burden_WHO_2017_ <- read_csv("Working_Data/healthcare/Vaccine Coverage and Disease Burden - WHO (2017).csv")
#View(Vaccine_Coverage_and_Disease_Burden_WHO_2017_)
Vaccine_Coverage_and_Disease_Burden_WHO_2017_df <- data.frame(Vaccine_Coverage_and_Disease_Burden_WHO_2017_)
Vaccine_Coverage_and_Disease_Burden_WHO_2017_df1 <- Vaccine_Coverage_and_Disease_Burden_WHO_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

# Create full healthcare and health economic dataframe
healthcare_data <- left_join(Vaccine_Coverage_and_Disease_Burden_WHO_2017_df1, df18, by = "Entity")

# Healthcare and health economic data parameters 
colnames(healthcare_data)

# Create .csv file for healthcare data 
write.csv(healthcare_data,"healthcare_data.csv", row.names = TRUE)

### DISEASE DATA ### 

Diabetes_Prevalence_NCD_RisC_2017_ <- read_csv("Working_Data/disease/Diabetes Prevalence - NCD RisC (2017).csv")
Diabetes_prevalence_relative_to_obesity_overweight_OWID_based_on_WHO_and_WDI <- read_csv("Working_Data/disease/Diabetes prevalence relative to obesity & overweight - OWID based on WHO and WDI.csv")
Diarrheal_disease_deaths_in_70_age_group_by_attributable_to_risk_factors_IHME_2018_ <- read_csv("Working_Data/disease/Diarrheal disease deaths in 70+ age group by attributable to risk factors (IHME 2018).csv")
Male_lung_cancer_deaths_per_100_000_WHO_IARC_2016_ <- read_csv("Working_Data/disease/Male lung cancer deaths per 100,000 - WHO (IARC) (2016).csv")
Non_communicable_disease_age_standardized_death_rates_IHME_2017_ <- read_csv("Working_Data/disease/Non-communicable disease age-standardized death rates - IHME (2017).csv")
Obesity_Prevalence_WHO_2019_ <- read_csv("Working_Data/disease/Obesity Prevalence - WHO (2019).csv")
Percentage_deaths_attributable_to_risk_factors_IHME <- read_csv("Working_Data/disease/Percentage deaths attributable to risk factors - IHME.csv")

updated_Diabetes_Prevalence_NCD_RisC_2017_ <- Diabetes_Prevalence_NCD_RisC_2017_ %>% group_by(Entity) %>% slice_max(Year)
d1 <- subset(updated_Diabetes_Prevalence_NCD_RisC_2017_, select=-c(Year))

updated_Diabetes_prevalence_relative_to_obesity_overweight_OWID_based_on_WHO_and_WDI <- Diabetes_prevalence_relative_to_obesity_overweight_OWID_based_on_WHO_and_WDI %>% group_by(Entity) %>% slice_max(Year)
d2 <- subset(updated_Diabetes_prevalence_relative_to_obesity_overweight_OWID_based_on_WHO_and_WDI, select=-c(Year))

updated_Diarrheal_disease_deaths_in_70_age_group_by_attributable_to_risk_factors_IHME_2018_ <- Diarrheal_disease_deaths_in_70_age_group_by_attributable_to_risk_factors_IHME_2018_ %>% group_by(Entity) %>% slice_max(Year)
d3 <- subset(updated_Diarrheal_disease_deaths_in_70_age_group_by_attributable_to_risk_factors_IHME_2018_, select=-c(Year))

updated_Male_lung_cancer_deaths_per_100_000_WHO_IARC_2016_ <- Male_lung_cancer_deaths_per_100_000_WHO_IARC_2016_ %>% group_by(Entity) %>% slice_max(Year)
d4 <- subset(updated_Male_lung_cancer_deaths_per_100_000_WHO_IARC_2016_, select=-c(Year))

updated_Non_communicable_disease_age_standardized_death_rates_IHME_2017_ <- Non_communicable_disease_age_standardized_death_rates_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
d5 <- subset(updated_Non_communicable_disease_age_standardized_death_rates_IHME_2017_, select=-c(Year))

updated_Obesity_Prevalence_WHO_2019_ <- Obesity_Prevalence_WHO_2019_ %>% group_by(Entity) %>% slice_max(Year)
d6 <- subset(updated_Obesity_Prevalence_WHO_2019_, select=-c(Year))

hold_1 <- merge(d1, d2, by = "Entity", all = TRUE)
hold_2 <- merge(d3, d4, by = "Entity", all = TRUE)
hold_3 <- merge(d5, d6, by = "Entity", all = TRUE)

hold_4 <- merge(hold_1, hold_2, by = "Entity", all = TRUE)
disease_df <- merge(hold_4, hold_3, by = "Entity", all = TRUE)

nrow(disease_df)
ncol(disease_df)

disease_df

write.csv(disease_df,"disease_data.csv", row.names = TRUE)

# Combine healthcare and disease data
healthcare_disease_data <- left_join(healthcare_data, disease_df, by = "Entity")

### FOOD AND WATER DATA ###

Affordability_of_diets_SOFI_2021_ <- read_csv("Working_Data/food_and_water/Affordability of diets (SOFI, 2021).csv")
Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI <- read_csv("Working_Data/food_and_water/Number of people with and without access to improved sanitation - OWID based on WDI.csv")

d1 <- subset(Affordability_of_diets_SOFI_2021_, select=-c(Year))
d2 <- Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI %>% group_by(Entity) %>% slice_max(Year)
d2 <- subset(d2, select=-c(Year))

food_water_df <- merge(d1, d2, by = "Entity", all = TRUE)
write.csv(food_water_df,"food_water_data.csv", row.names = TRUE)

# Combine healthcare, disease, food and water data

healthcare_disease_foodwater_data <- left_join(healthcare_disease_data, food_water_df, by = "Entity")

### WORLD STATS DATA ###

World_Happiness_Report_2021_ <- read_csv("Working_Data/world_stats/World Happiness Report (2021).csv")

worldstats_data <- World_Happiness_Report_2021_ %>% group_by(Entity) %>% slice_max(Year)
worldstats_data <- subset(worldstats_data, select=-c(Year))
write.csv(worldstats_data,"happyness_combined.csv", row.names = TRUE)

# Combine healthcare, disease, food and water, world stats data

healthcare_disease_foodwater_worldstats_data <- left_join(healthcare_disease_foodwater_data, worldstats_data, by = "Entity")

### COVID DATA ### 

COVID_Government_Response_OxBSG_ <- read_csv("Working_Data/covid/COVID Government Response (OxBSG).csv")
COVID_Government_Response_OxBSG_df <- data.frame(COVID_Government_Response_OxBSG_) %>% group_by(Entity) %>% slice_max(Year) 
COVID_Government_Response_OxBSG_df_NO_YEAR <- subset(COVID_Government_Response_OxBSG_df, select= -c(Year))

COVID_testing_time_series_data <- read_csv("Working_Data/covid/COVID testing time series data.csv")
COVID_testing_time_series_data_df <- data.frame(COVID_testing_time_series_data) %>% group_by(Entity) %>% slice_max(Year) 
COVID_testing_time_series_data_df_NO_YEAR <- subset(COVID_testing_time_series_data_df, select= -c(Year))

COVID_19_Variants <- read_csv("Working_Data/covid/COVID-19 - Variants.csv")
COVID_19_Variants_df <- data.frame(COVID_19_Variants) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Variants_df_NO_YEAR <- subset(COVID_19_Variants_df, select= -c(Year))

COVID_19_Tests_per_million_people <- read_csv("Working_Data/covid/COVID-19 Tests per million people.csv")
COVID_19_Tests_per_million_people_df <- data.frame(COVID_19_Tests_per_million_people) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Tests_per_million_people_df_NO_YEAR <- subset(COVID_19_Tests_per_million_people_df, select= -c(Year))

COVID_19_Tests <- read_csv("Working_Data/covid/COVID-19 Tests.csv")
COVID_19_Tests_df <- data.frame(COVID_19_Tests) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Tests_df_NO_YEAR <- subset(COVID_19_Tests_df, select= -c(Year))

COVID_2019_ECDC_2020_ <- read_csv("Working_Data/covid/COVID-2019 - ECDC (2020).csv")
COVID_2019_ECDC_2020_df <- data.frame(COVID_2019_ECDC_2020_) %>% group_by(Entity) %>% slice_max(Year) 
COVID_2019_ECDC_2020_df_NO_YEAR <- subset(COVID_2019_ECDC_2020_df, select= -c(Year))

COVID_2019_Hospital_ICU <- read_csv("Working_Data/covid/COVID-2019 - Hospital & ICU.csv")
COVID_2019_Hospital_ICU_df <- data.frame(COVID_2019_Hospital_ICU) %>% group_by(Entity) %>% slice_max(Year) 
COVID_2019_Hospital_ICU_df_NO_YEAR <- subset(COVID_2019_Hospital_ICU_df, select= -c(Year))

State_of_Vaccine_Confidence_Larson_et_al_2016_ <- read_csv("Working_Data/covid/State of Vaccine Confidence - Larson et al (2016).csv")
State_of_Vaccine_Confidence_Larson_et_al_2016_df <- data.frame(State_of_Vaccine_Confidence_Larson_et_al_2016_) %>% group_by(Entity) %>% slice_max(Year) 
State_of_Vaccine_Confidence_Larson_et_al_2016_df_NO_YEAR <- subset(State_of_Vaccine_Confidence_Larson_et_al_2016_df, select= -c(Year))

# Covid dataframes to merge to single dataframe

merge_1.1 <- merge(x = COVID_19_Tests_df_NO_YEAR, 
                   y = COVID_19_Tests_per_million_people_df_NO_YEAR , by = "Entity", all = TRUE)

merge_1.2 <- merge(x = COVID_19_Variants_df_NO_YEAR, 
                   y = COVID_2019_ECDC_2020_df_NO_YEAR , by = "Entity", all = TRUE)

merge_1.3 <- merge(x = COVID_2019_Hospital_ICU_df_NO_YEAR, 
                   y = COVID_Government_Response_OxBSG_df_NO_YEAR , by = "Entity", all = TRUE)

merge_1.4 <- merge(x = COVID_testing_time_series_data_df_NO_YEAR, 
                   y = State_of_Vaccine_Confidence_Larson_et_al_2016_df_NO_YEAR , by = "Entity", all = TRUE)

merge_2.1 <- merge(x = merge_1.1, 
                   y = merge_1.2 , by = "Entity", all = TRUE)

merge_2.2 <- merge(x = merge_1.3, 
                   y = merge_1.4 , by = "Entity", all = TRUE)

Covid_combined_data <- merge(x = merge_2.1, 
                             y = merge_2.2 , by = "Entity", all = TRUE)

#View(Covid_combined_data)

# Create .csv file for covid dataframe 

write.csv(Covid_combined_data, "Covid_combined_data.csv", row.names = TRUE)

# Combine healthcare, disease, food and water, world stats, covid data
hc_dis_fw_ws_covid_data <- left_join(healthcare_disease_foodwater_worldstats_data, Covid_combined_data, by = "Entity")

### ENVIRONMENTAL DATA ### 

# Environmental data import and convert to dataframe

Air_pollution_deaths_breakdown_by_age_IHME <- read_csv("Working_Data/environmental/Air pollution deaths breakdown by age - IHME.csv")
Air_pollution_deaths_breakdown_by_age_IHME_df <- data.frame(Air_pollution_deaths_breakdown_by_age_IHME)

Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_ <- read_csv("Working_Data/environmental/Deaths attributed to air pollution (Lelieveld et al. 2019).csv")
Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df <- data.frame(Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_)

Air_pollution_deaths_breakdown_by_age_IHME_df <- Air_pollution_deaths_breakdown_by_age_IHME_df %>% group_by(Entity) %>% slice_max(Year) 
Air_pollution_deaths_breakdown_by_age_IHME_df_NO_YEAR <- subset(Air_pollution_deaths_breakdown_by_age_IHME_df, select= -c(Year))

Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df <- Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df %>% group_by(Entity) %>% slice_max(Year)
Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df_NO_YEAR <- subset(Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df, select= -c(Year))

Environmental_data <- merge(x = Air_pollution_deaths_breakdown_by_age_IHME_df_NO_YEAR, 
                            y = Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df_NO_YEAR , by = "Entity", all = TRUE)
#View(Environmental_data)

# Create .csv file for dataframe for environmental data 

write.csv(Environmental_data, "Environmental_data.csv", row.names = TRUE)

# Combine healthcare, disease, food and water, world stats, covid, environmental data
hc_dis_fw_ws_covid_env_data <- left_join(hc_dis_fw_ws_covid_data, Environmental_data, by = "Entity")

### DEMOGRAPHIC DATA ### 

Child_mortality_estimates_Gapminder_2015_ <- read_csv("Working_Data/demographics/Child mortality estimates - Gapminder (2015).csv")
#View(Child_mortality_estimates_Gapminder_2015_)
Child_mortality_estimates_Gapminder_2015_df <- data.frame(Child_mortality_estimates_Gapminder_2015_)
Child_mortality_estimates_Gapminder_2015_df1 <- Child_mortality_estimates_Gapminder_2015_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

Child_mortality_1950_2017_IHME_2017_ <- read_csv("Working_Data/demographics/Child mortality, 1950-2017 (IHME, 2017).csv")
#View(Child_mortality_1950_2017_IHME_2017_)
Child_mortality_1950_2017_IHME_2017_df <- data.frame(Child_mortality_1950_2017_IHME_2017_)
Child_mortality_1950_2017_IHME_2017_df1 <- Child_mortality_1950_2017_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df1demo <- left_join(Child_mortality_estimates_Gapminder_2015_df1, Child_mortality_1950_2017_IHME_2017_df1, by = "Entity")

Deaths_by_World_Region_WHO_2016_ <- read_csv("Working_Data/demographics/Deaths by World Region - WHO (2016).csv")
#View(Deaths_by_World_Region_WHO_2016_)
Deaths_by_World_Region_WHO_2016_df <- data.frame(Deaths_by_World_Region_WHO_2016_)
Deaths_by_World_Region_WHO_2016_df1 <- Deaths_by_World_Region_WHO_2016_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df2demo <- left_join(Deaths_by_World_Region_WHO_2016_df1, df1demo, by = "Entity")

Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau <- read_csv("Working_Data/demographics/Differences in population estimates - OWID based on UN vs US Census Bureau.csv")
#View(Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau)
Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau_df <- data.frame(Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau)
Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau_df1 <- Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df3demo <- left_join(Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau_df1, df2demo, by = "Entity")

Excess_Mortality_Data_OWID_2021_ <- read_csv("Working_Data/demographics/Excess Mortality Data – OWID (2021).csv")
#View(Excess_Mortality_Data_OWID_2021_)
Excess_Mortality_Data_OWID_2021_df <- data.frame(Excess_Mortality_Data_OWID_2021_)
Excess_Mortality_Data_OWID_2021_df1 <- Excess_Mortality_Data_OWID_2021_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df4demo <- left_join(Excess_Mortality_Data_OWID_2021_df1, df3demo, by = "Entity")

Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_ <- read_csv("Working_Data/demographics/Female and male life expectancy at birth - OWID based on UN Population Division (2017).csv")
#View(Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_)
Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_df <- data.frame(Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_)
Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_df1 <- Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df5demo <- left_join(Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_df1, df4demo, by = "Entity")

Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_ <- read_csv("Working_Data/demographics/Global child mortality (since 1800) - based on Gapminder and World Bank (2019).csv")
#View(Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_)
Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_df <- data.frame(Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_)
Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_df1 <- Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df6demo <- left_join(Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_df1, df5demo, by = "Entity")

Inequality_in_human_development_indices_UNDP_2019_ <- read_csv("Working_Data/demographics/Inequality in human development indices - UNDP (2019).csv")
#View(Inequality_in_human_development_indices_UNDP_2019_)
Inequality_in_human_development_indices_UNDP_2019_df <- data.frame(Inequality_in_human_development_indices_UNDP_2019_) 
Inequality_in_human_development_indices_UNDP_2019_df1 <- Inequality_in_human_development_indices_UNDP_2019_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df7demo <- left_join(Inequality_in_human_development_indices_UNDP_2019_df1, df6demo, by = "Entity")

Infant_mortality_rate_IHME_2017_ <- read_csv("Working_Data/demographics/Infant mortality rate (IHME - 2017).csv")
#View(Infant_mortality_rate_IHME_2017_)
Infant_mortality_rate_IHME_2017_df <- data.frame(Infant_mortality_rate_IHME_2017_)
Infant_mortality_rate_IHME_2017_df1 <- Infant_mortality_rate_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df8demo <- left_join(Infant_mortality_rate_IHME_2017_df1, df7demo, by = "Entity")

International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_ <- read_csv("Working_Data/demographics/International Historical Statistics (Deaths per 1,000) - Brian Mitchell (2013).csv")
#View(International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_)
International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_df <- data.frame(International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_)
International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_df1 <- International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df9demo <- left_join(International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_df1, df8demo, by = "Entity")

Life_Expectancy_1950_2015_UN_Population_Division_2015_ <- read_csv("Working_Data/demographics/Life Expectancy (1950-2015) – UN Population Division (2015).csv")
#View(Life_Expectancy_1950_2015_UN_Population_Division_2015_)
Life_Expectancy_1950_2015_UN_Population_Division_2015_df <- data.frame(Life_Expectancy_1950_2015_UN_Population_Division_2015_)
Life_Expectancy_1950_2015_UN_Population_Division_2015_df1 <- Life_Expectancy_1950_2015_UN_Population_Division_2015_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df10demo <- left_join(Life_Expectancy_1950_2015_UN_Population_Division_2015_df1, df9demo, by = "Entity")

Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_ <- read_csv("Working_Data/demographics/Life expectancy & population - Gapminder (2019), UN (2019), and Our World In Data (2019).csv")
#View(Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_)
Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_df <- data.frame(Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_)
Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_df1 <- Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df11demo <- left_join(Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_df1, df10demo, by = "Entity")

Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_ <- read_csv("Working_Data/demographics/Male and female life expectancy by age in the long run (Human Mortality Database (2018) and others).csv")
#View(Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_)
Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_df <- data.frame(Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_)
Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_df1 <- Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df12demo <- left_join(Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_df1, df11demo, by = "Entity")

Mean_BMI_NCD_RisC_2017_ <- read_csv("Working_Data/demographics/Mean BMI - NCD RisC (2017).csv")
#View(Mean_BMI_NCD_RisC_2017_)
Mean_BMI_NCD_RisC_2017_df <- data.frame(Mean_BMI_NCD_RisC_2017_)
Mean_BMI_NCD_RisC_2017_df1 <- Mean_BMI_NCD_RisC_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df13demo <- left_join(Mean_BMI_NCD_RisC_2017_df1, df12demo, by = "Entity")

Neonatal_Mortality_Rate_via_Childmortality_org_2015_ <- read_csv("Working_Data/demographics/Neonatal Mortality Rate - via Childmortality.org (2015).csv")
#View(Neonatal_Mortality_Rate_via_Childmortality_org_2015_)
Neonatal_Mortality_Rate_via_Childmortality_org_2015_df <- data.frame(Neonatal_Mortality_Rate_via_Childmortality_org_2015_)
Neonatal_Mortality_Rate_via_Childmortality_org_2015_df1 <- Neonatal_Mortality_Rate_via_Childmortality_org_2015_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df14demo <- left_join(Neonatal_Mortality_Rate_via_Childmortality_org_2015_df1, df13demo, by = "Entity")

Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_ <- read_csv("Working_Data/demographics/Number and percentage of current smokers, by sex (American Lung Association (2011)).csv")
#View(Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_)
Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_df <- data.frame(Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_)
Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_df1 <- Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df15demo <- left_join(Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_df1, df14demo, by = "Entity")

Number_of_child_deaths_1950_2017_IHME_2017_ <- read_csv("Working_Data/demographics/Number of child deaths 1950-2017 (IHME, 2017).csv")
#View(Number_of_child_deaths_1950_2017_IHME_2017_)
Number_of_child_deaths_1950_2017_IHME_2017_df <- data.frame(Number_of_child_deaths_1950_2017_IHME_2017_)
Number_of_child_deaths_1950_2017_IHME_2017_df1 <- Number_of_child_deaths_1950_2017_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df16demo <- left_join(Number_of_child_deaths_1950_2017_IHME_2017_df1, df15demo, by = "Entity")

Number_of_infant_deaths_IHME_2017_ <- read_csv("Working_Data/demographics/Number of infant deaths (IHME - 2017).csv")
#View(Number_of_infant_deaths_IHME_2017_)
Number_of_infant_deaths_IHME_2017_df <- data.frame(Number_of_infant_deaths_IHME_2017_)
Number_of_infant_deaths_IHME_2017_df1 <- Number_of_infant_deaths_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df17demo <- left_join(Number_of_infant_deaths_IHME_2017_df1, df16demo, by = "Entity")

Number_of_neonatal_deaths_IHME_2017_ <- read_csv("Working_Data/demographics/Number of neonatal deaths (IHME - 2017).csv")
#View(Number_of_neonatal_deaths_IHME_2017_)
Number_of_neonatal_deaths_IHME_2017_df <- data.frame(Number_of_neonatal_deaths_IHME_2017_)
Number_of_neonatal_deaths_IHME_2017_df1 <- Number_of_neonatal_deaths_IHME_2017_df %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

df18demo <- left_join(Number_of_neonatal_deaths_IHME_2017_df1, df17demo, by = "Entity")

Population_Gapminder_HYDE_UN_ <- read_csv("Working_Data/demographics/Population (Gapminder, HYDE & UN).csv")
#View(Population_Gapminder_HYDE_UN_)
Population_Gapminder_HYDE_UN_df <- data.frame(Population_Gapminder_HYDE_UN_)
Population_Gapminder_HYDE_UN_df1 <- Population_Gapminder_HYDE_UN_df  %>%
  group_by(Entity) %>%
  slice_max(Year) %>% 
  select(-Year)

demo_data <- left_join(Population_Gapminder_HYDE_UN_df1, df18demo, by = "Entity")

# Combine healthcare, disease, food and water, world stats, covid, environmental, demographic data
# i.e. all data except economic data 

hc_dis_fw_ws_covid_env_demo_data <- left_join(hc_dis_fw_ws_covid_env_data, Environmental_data, by = "Entity")

write.csv(hc_dis_fw_ws_covid_env_demo_data,"initial_final_df.csv", row.names = TRUE)


