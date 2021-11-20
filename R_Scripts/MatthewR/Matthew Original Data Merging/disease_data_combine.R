library("ggplot2")
library("tidyverse")
library("readr")

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
