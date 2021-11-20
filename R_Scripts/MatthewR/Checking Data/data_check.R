# Trying to sort the data into a better data.frame.

library("ggplot2")
library("tidyverse")
library("readr")

# Disease Data

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

# Food and Water Data

Affordability_of_diets_SOFI_2021_ <- read_csv("Working_Data/food_and_water/Affordability of diets (SOFI, 2021).csv")
Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI <- read_csv("Working_Data/food_and_water/Number of people with and without access to improved sanitation - OWID based on WDI.csv")

d1 <- subset(Affordability_of_diets_SOFI_2021_, select=-c(Year))
d2 <- Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI %>% group_by(Entity) %>% slice_max(Year)
d2 <- subset(d2, select=-c(Year))

food_water_df <- merge(d1, d2, by = "Entity", all = TRUE)

# World Stats Data - only have one csv source for this anyway.

World_Happiness_Report_2021_ <- read_csv("Working_Data/world_stats/World Happiness Report (2021).csv")

ws2 <- World_Happiness_Report_2021_ %>% group_by(Entity) %>% slice_max(Year)
ws2 <- subset(ws2, select=-c(Year))

# Covid Data

COVID_Government_Response_OxBSG_ <- read_csv("Working_Data/covid/COVID Government Response (OxBSG).csv")
updated_COVID_Government_Response_OxBSG_ <- COVID_Government_Response_OxBSG_ %>% group_by(Entity) %>% slice_max(Year)
c1 <- subset(updated_COVID_Government_Response_OxBSG_, select=-c(Year))

COVID_testing_time_series_data <- read_csv("Working_Data/covid/COVID testing time series data.csv")
updated_COVID_testing_time_series_data <- COVID_testing_time_series_data %>% group_by(Entity) %>% slice_max(Year)
c2 <- subset(updated_COVID_testing_time_series_data, select=-c(Year,annotation))

COVID_19_Variants <- read_csv("Working_Data/covid/COVID-19 - Variants.csv")
updated_COVID_19_Variants <- COVID_19_Variants %>% group_by(Entity) %>% slice_max(Year)
c3 <- subset(updated_COVID_19_Variants, select=-c(Year))

COVID_19_Tests_per_million_people <- read_csv("Working_Data/covid/COVID-19 Tests per million people.csv")
updated_COVID_19_Tests_per_million_people <- COVID_19_Tests_per_million_people %>% group_by(Entity) %>% slice_max(Year)
c4 <- subset(updated_COVID_19_Tests_per_million_people, select=-c(Year))

COVID_19_Tests <- read_csv("Working_Data/covid/COVID-19 Tests.csv")
updated_COVID_19_Tests <- COVID_19_Tests %>% group_by(Entity) %>% slice_max(Year)
c5 <- subset(updated_COVID_19_Tests, select=-c(Year))


COVID_2019_ECDC_2020_ <- read_csv("Working_Data/covid/COVID-2019 - ECDC (2020).csv")
updated_COVID_2019_ECDC_2020_ <- COVID_2019_ECDC_2020_ %>% group_by(Entity) %>% slice_max(Year)
c6 <- subset(updated_COVID_2019_ECDC_2020_, select=-c(Year))

COVID_2019_Hospital_ICU <- read_csv("Working_Data/covid/COVID-2019 - Hospital & ICU.csv")
updated_COVID_2019_Hospital_ICU <- COVID_2019_Hospital_ICU %>% group_by(Entity) %>% slice_max(Year)
c7 <- subset(updated_COVID_2019_Hospital_ICU, select=-c(Year))

State_of_Vaccine_Confidence_Larson_et_al_2016_ <- read_csv("Working_Data/covid/State of Vaccine Confidence - Larson et al (2016).csv")
updated_State_of_Vaccine_Confidence_Larson_et_al_2016_ <- State_of_Vaccine_Confidence_Larson_et_al_2016_ %>% group_by(Entity) %>% slice_max(Year)
c8 <- subset(updated_State_of_Vaccine_Confidence_Larson_et_al_2016_, select=-c(Year))

covid_hold_1 <- merge(c1, d2, by = "Entity", all = TRUE)
covid_hold_2 <- merge(c3, c4, by = "Entity", all = TRUE)
covid_hold_3 <- merge(c5, c6, by = "Entity", all = TRUE)
covid_hold_4 <- merge(c7, c8, by = "Entity", all = TRUE)

c_h_2_1 <- merge(covid_hold_1, covid_hold_2, by = "Entity", all = TRUE)
c_h_2_2 <- merge(covid_hold_3, covid_hold_4, by = "Entity", all = TRUE)

covid_df <- merge(c_h_2_1, c_h_2_2, by = "Entity", all = TRUE)

# Demographic Data

# Have not included these datasets as no country data - just continents.

Deaths_by_World_Region_WHO_2016_ <- read_csv("Working_Data/demographics/Deaths by World Region - WHO (2016).csv")
Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau <- read_csv("Working_Data/demographics/Differences in population estimates - OWID based on UN vs US Census Bureau.csv")
Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_ <- read_csv("Working_Data/demographics/Global child mortality (since 1800) - based on Gapminder and World Bank (2019).csv")
Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_ <- read_csv("Working_Data/demographics/Number and percentage of current smokers, by sex (American Lung Association (2011)).csv")


# Now these ones are included.

Child_mortality_estimates_Gapminder_2015_ <- read_csv("Working_Data/demographics/Child mortality estimates - Gapminder (2015).csv")
updated_Child_mortality_estimates_Gapminder_2015_ <- Child_mortality_estimates_Gapminder_2015_  %>% group_by(Entity) %>% slice_max(Year)
de1 <- subset(updated_Child_mortality_estimates_Gapminder_2015_, select=-c(Year))

Child_mortality_1950_2017_IHME_2017_ <- read_csv("Working_Data/demographics/Child mortality, 1950-2017 (IHME, 2017).csv")
updated_Child_mortality_1950_2017_IHME_2017_ <- Child_mortality_1950_2017_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de2 <- subset(updated_Child_mortality_1950_2017_IHME_2017_, select=-c(Year) )



Excess_Mortality_Data_OWID_2021_ <- read_csv("Working_Data/demographics/Excess Mortality Data – OWID (2021).csv")
updated_Excess_Mortality_Data_OWID_2021_ <- Child_mortality_1950_2017_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de3 <- subset(updated_Excess_Mortality_Data_OWID_2021_, select=-c(Year) )

Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_ <- read_csv("Working_Data/demographics/Female and male life expectancy at birth - OWID based on UN Population Division (2017).csv")
updated_Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_ <- Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_ %>% group_by(Entity) %>% slice_max(Year)
de4 <- subset(updated_Female_and_male_life_expectancy_at_birth_OWID_based_on_UN_Population_Division_2017_, select=-c(Year) )


Inequality_in_human_development_indices_UNDP_2019_ <- read_csv("Working_Data/demographics/Inequality in human development indices - UNDP (2019).csv")
updated_Inequality_in_human_development_indices_UNDP_2019_ <- Inequality_in_human_development_indices_UNDP_2019_ %>% group_by(Entity) %>% slice_max(Year)
de4 <- subset(updated_Inequality_in_human_development_indices_UNDP_2019_, select=-c(Year) )

Infant_mortality_rate_IHME_2017_ <- read_csv("Working_Data/demographics/Infant mortality rate (IHME - 2017).csv")
updated_Infant_mortality_rate_IHME_2017_ <- Infant_mortality_rate_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de5 <- subset(updated_Infant_mortality_rate_IHME_2017_, select=-c(Year) )

International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_ <- read_csv("Working_Data/demographics/International Historical Statistics (Deaths per 1,000) - Brian Mitchell (2013).csv")
updated_International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_ <- International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_ %>% group_by(Entity) %>% slice_max(Year)
de6 <- subset(updated_International_Historical_Statistics_Deaths_per_1_000_Brian_Mitchell_2013_, select=-c(Year) )


Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_ <- read_csv("Working_Data/demographics/Life expectancy & population - Gapminder (2019), UN (2019), and Our World In Data (2019).csv")
updated_Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_ <- Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_ %>% group_by(Entity) %>% slice_max(Year)
de7 <- subset(updated_Life_expectancy_population_Gapminder_2019_UN_2019_and_Our_World_In_Data_2019_, select=-c(Year) )

Life_Expectancy_1950_2015_UN_Population_Division_2015_ <- read_csv("Working_Data/demographics/Life Expectancy (1950-2015) – UN Population Division (2015).csv")
updated_Life_Expectancy_1950_2015_UN_Population_Division_2015_ <- Life_Expectancy_1950_2015_UN_Population_Division_2015_ %>% group_by(Entity) %>% slice_max(Year)
de8 <- subset(updated_Life_Expectancy_1950_2015_UN_Population_Division_2015_, select=-c(Year) )

Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_ <- read_csv("Working_Data/demographics/Male and female life expectancy by age in the long run (Human Mortality Database (2018) and others).csv")
updated_Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_ <- Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_ %>% group_by(Entity) %>% slice_max(Year)
de9 <- subset(updated_Male_and_female_life_expectancy_by_age_in_the_long_run_Human_Mortality_Database_2018_and_others_, select=-c(Year) )

Mean_BMI_NCD_RisC_2017_ <- read_csv("Working_Data/demographics/Mean BMI - NCD RisC (2017).csv")
updated_Mean_BMI_NCD_RisC_2017_ <- Mean_BMI_NCD_RisC_2017_ %>% group_by(Entity) %>% slice_max(Year)
de10 <- subset(updated_Mean_BMI_NCD_RisC_2017_, select=-c(Year))

Neonatal_Mortality_Rate_via_Childmortality_org_2015_ <- read_csv("Working_Data/demographics/Neonatal Mortality Rate - via Childmortality.org (2015).csv")
updated_Neonatal_Mortality_Rate_via_Childmortality_org_2015_ <- Neonatal_Mortality_Rate_via_Childmortality_org_2015_ %>% group_by(Entity) %>% slice_max(Year)
de11 <- subset(updated_Neonatal_Mortality_Rate_via_Childmortality_org_2015_, select=-c(Year) )

Number_of_child_deaths_1950_2017_IHME_2017_ <- read_csv("Working_Data/demographics/Number of child deaths 1950-2017 (IHME, 2017).csv")
updated_Number_of_child_deaths_1950_2017_IHME_2017_ <- Number_of_child_deaths_1950_2017_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de12 <- subset(updated_Number_of_child_deaths_1950_2017_IHME_2017_, select=-c(Year) )

Number_of_infant_deaths_IHME_2017_ <- read_csv("Working_Data/demographics/Number of infant deaths (IHME - 2017).csv")
updated_Number_of_infant_deaths_IHME_2017_ <- Number_of_infant_deaths_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de13 <- subset(updated_Number_of_infant_deaths_IHME_2017_, select=-c(Year))


Number_of_neonatal_deaths_IHME_2017_ <- read_csv("Working_Data/demographics/Number of neonatal deaths (IHME - 2017).csv")
updated_Number_of_neonatal_deaths_IHME_2017_ <- Number_of_neonatal_deaths_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
de14 <- subset(updated_Number_of_neonatal_deaths_IHME_2017_, select=-c(Year))

Population_Gapminder_HYDE_UN_ <- read_csv("Working_Data/demographics/Population (Gapminder, HYDE & UN).csv")
updated_Population_Gapminder_HYDE_UN_ <- Population_Gapminder_HYDE_UN_ %>% group_by(Entity) %>% slice_max(Year)
de15 <- subset(updated_Population_Gapminder_HYDE_UN_, select=-c(Year))

de_h1 <- merge(de1, de2, by = "Entity", all = TRUE)
de_h2 <- merge(de3, de4, by = "Entity", all = TRUE)
de_h3 <- merge(de5, de6, by = "Entity", all = TRUE)
de_h4 <- merge(de7, de8, by = "Entity", all = TRUE)
de_h5 <- merge(de9, de10, by = "Entity", all = TRUE)
de_h6 <- merge(de11, de12, by = "Entity", all = TRUE)
de_h7 <- merge(de13, de14, by = "Entity", all = TRUE)

dhold1 <- merge(de_h1, de_h2, by = "Entity", all = TRUE)
dhold2 <- merge(de_h3, de_h4, by = "Entity", all = TRUE)
dhold3 <- merge(de_h5, de_h6, by = "Entity", all = TRUE)
dhold4 <- merge(de_h7, de15, by = "Entity", all = TRUE)


dhold1_1 <- merge(dhold1, dhold2, by = "Entity", all = TRUE)
dhold1_2 <- merge(dhold3, dhold4, by = "Entity", all = TRUE)

demographic_df <- merge(dhold1_1, dhold1_2, by = "Entity", all = TRUE)







