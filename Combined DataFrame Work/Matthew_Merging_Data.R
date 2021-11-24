# Trying to sort the data into a better data.frame.

library("ggplot2")
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)


# Firstly, have not included these datasets as no country data - just continents/world/regions etc!

Deaths_by_World_Region_WHO_2016_ <- read_csv("Working_Data/demographics/Deaths by World Region - WHO (2016).csv")
Differences_in_population_estimates_OWID_based_on_UN_vs_US_Census_Bureau <- read_csv("Working_Data/demographics/Differences in population estimates - OWID based on UN vs US Census Bureau.csv")
Global_child_mortality_since_1800_based_on_Gapminder_and_World_Bank_2019_ <- read_csv("Working_Data/demographics/Global child mortality (since 1800) - based on Gapminder and World Bank (2019).csv")
Number_and_percentage_of_current_smokers_by_sex_American_Lung_Association_2011_ <- read_csv("Working_Data/demographics/Number and percentage of current smokers, by sex (American Lung Association (2011)).csv")
GDP_per_capita_indexed_at_1950_Maddison_Project_Data_2018_ <- read_csv("Working_Data/economic/GDP per capita indexed at 1950 - Maddison Project Data (2018).csv")
Wealth_total_by_component_for_various_country_groupings_World_Bank_2017_ <- read_csv("Working_Data/economic/Wealth (total) by component for various country groupings - World Bank (2017).csv")
Wealth_per_capita_by_component_for_various_country_groupings_World_Bank_2017_ <- read_csv("Working_Data/economic/Wealth per capita by component for various country groupings - World Bank (2017).csv")
World_GDP_in_2011_int_OWID_based_on_World_Bank_Maddison_2017_ <- read_csv("Working_Data/economic/World GDP in 2011 int $ – OWID based on World Bank + Maddison (2017).csv")
Development_Health_Assistance_IHME <- read_csv("Working_Data/healthcare/Development Health Assistance - IHME.csv")

# Now for merging the data from each category.

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
world_stats <- subset(ws2, select=-c(Year))

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


# Environmental Data

Air_pollution_deaths_breakdown_by_age_IHME <- read_csv("Working_Data/environmental/Air pollution deaths breakdown by age - IHME.csv")
updated_Air_pollution_deaths_breakdown_by_age_IHME <- Air_pollution_deaths_breakdown_by_age_IHME %>% group_by(Entity) %>% slice_max(Year)
en_1 <- subset(updated_Air_pollution_deaths_breakdown_by_age_IHME, select=-c(Year))

Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_ <- read_csv("Working_Data/environmental/Deaths attributed to air pollution (Lelieveld et al. 2019).csv")
updated_Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_ <- Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_ %>% group_by(Entity) %>% slice_max(Year)
en_2 <- subset(updated_Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_, select=-c(Year))

environment_df <- merge(en_1, en_2, by = "Entity", all = TRUE)

# Economic Data

Average_monthly_incomes_or_consumption_by_decile_and_quintile_PovcalNet_2019_ <- read_csv("Working_Data/economic/Average monthly incomes or consumption by decile and quintile – PovcalNet (2019).csv")
updated_Average_monthly_incomes_or_consumption_by_decile_and_quintile_PovcalNet_2019_ <- Average_monthly_incomes_or_consumption_by_decile_and_quintile_PovcalNet_2019_ %>% group_by(Entity) %>% slice_max(Year)
ec_1 <- subset(updated_Average_monthly_incomes_or_consumption_by_decile_and_quintile_PovcalNet_2019_, select=-c(Year, welfare))

Country_Income_Classification_World_Bank_2017_ <- read_csv("Working_Data/economic/Country Income Classification - World Bank (2017).csv")
updated_Country_Income_Classification_World_Bank_2017_ <- Country_Income_Classification_World_Bank_2017_ %>% group_by(Entity) %>% slice_max(Year)
ec_2 <- subset(updated_Country_Income_Classification_World_Bank_2017_, select=-c(Year))

economic_inequality_gini_index <- read_csv("Working_Data/economic/economic-inequality-gini-index.csv")
updated_economic_inequality_gini_index <- economic_inequality_gini_index %>% group_by(Entity) %>% slice_max(Year)
ec_3 <- subset(updated_economic_inequality_gini_index, select=-c(Year, Code))

GDP_growth_from_previous_year_2020_Q2_Eurostat_OECD_National_sources_ <- read_csv("Working_Data/economic/GDP growth from previous year, 2020 Q2 (Eurostat, OECD, National sources).csv")
updated_GDP_growth_from_previous_year_2020_Q2_Eurostat_OECD_National_sources_ <- GDP_growth_from_previous_year_2020_Q2_Eurostat_OECD_National_sources_ %>% group_by(Entity) %>% slice_max(Year)
ec_4 <- subset(updated_GDP_growth_from_previous_year_2020_Q2_Eurostat_OECD_National_sources_, select=-c(Year))

GDP_per_capita_PPP_2011_WDI_2016_ <- read_csv("Working_Data/economic/GDP per capita PPP 2011 – WDI (2016).csv")
updated_GDP_per_capita_PPP_2011_WDI_2016_ <- GDP_per_capita_PPP_2011_WDI_2016_ %>% group_by(Entity) %>% slice_max(Year)
ec_5 <- subset(updated_GDP_per_capita_PPP_2011_WDI_2016_, select=-c(Year))

GNP_by_country_2021 <- read_csv("Working_Data/economic/GNP by country 2021.csv")
updated_GNP_by_country_2021 <- GNP_by_country_2021 %>% group_by(country)
ec_6 <- subset(updated_GNP_by_country_2021, select=-c(rank))
ec_6 <- rename(ec_6,Entity = country)

Income_Inequality_World_Bank_2016_ <- read_csv("Working_Data/economic/Income Inequality - World Bank (2016).csv")
updated_Income_Inequality_World_Bank_2016_ <- Income_Inequality_World_Bank_2016_ %>% group_by(Entity) %>% slice_max(Year)
ec_7 <- subset(updated_Income_Inequality_World_Bank_2016_, select=-c(Year))

Maddison_Project_Database_2020_Bolt_and_van_Zanden_2020_ <- read_csv("Working_Data/economic/Maddison Project Database 2020 (Bolt and van Zanden (2020)).csv")
updated_Maddison_Project_Database_2020_Bolt_and_van_Zanden_2020_ <- Maddison_Project_Database_2020_Bolt_and_van_Zanden_2020_ %>% group_by(Entity) %>% slice_max(Year)
ec_8 <- subset(updated_Maddison_Project_Database_2020_Bolt_and_van_Zanden_2020_, select=-c(Year))

Measures_and_indicators_for_Poverty_PovcalNet_World_Bank_2017_ <- read_csv("Working_Data/economic/Measures and indicators for Poverty - PovcalNet (World Bank) (2017).csv")
updated_Measures_and_indicators_for_Poverty_PovcalNet_World_Bank_2017_ <- Measures_and_indicators_for_Poverty_PovcalNet_World_Bank_2017_ %>% group_by(Entity) %>% slice_max(Year)
ec_9 <- subset(updated_Measures_and_indicators_for_Poverty_PovcalNet_World_Bank_2017_, select=-c(Year))

National_Poverty_Lines_Jolliffe_and_Prydz_2016_ <- read_csv("Working_Data/economic/National Poverty Lines - Jolliffe and Prydz (2016).csv")
updated_National_Poverty_Lines_Jolliffe_and_Prydz_2016_ <- National_Poverty_Lines_Jolliffe_and_Prydz_2016_ %>% group_by(Entity) %>% slice_max(Year)
ec_10 <- subset(updated_National_Poverty_Lines_Jolliffe_and_Prydz_2016_, select=-c(Year))

OPHI_Multidimensional_Poverty_Index_Alkire_and_Robles_2016_ <- read_csv("Working_Data/economic/OPHI Multidimensional Poverty Index - Alkire and Robles (2016).csv")
updated_OPHI_Multidimensional_Poverty_Index_Alkire_and_Robles_2016_ <- OPHI_Multidimensional_Poverty_Index_Alkire_and_Robles_2016_ %>% group_by(Entity) %>% slice_max(Year)
ec_11 <- subset(updated_OPHI_Multidimensional_Poverty_Index_Alkire_and_Robles_2016_, select=-c(Year))

PolcalNet_Global_Poverty_2017_ <- read_csv("Working_Data/economic/PolcalNet Global Poverty (2017).csv")
updated_PolcalNet_Global_Poverty_2017_ <- PolcalNet_Global_Poverty_2017_ %>% group_by(Entity) %>% slice_max(Year)
ec_12 <- subset(updated_PolcalNet_Global_Poverty_2017_, select=-c(Year))

Poverty_rate_50_of_median_LIS_Key_Figures_2018_ <- read_csv("Working_Data/economic/Poverty rate (!50% of median) (LIS Key Figures, 2018).csv")
updated_Poverty_rate_50_of_median_LIS_Key_Figures_2018_ <- Poverty_rate_50_of_median_LIS_Key_Figures_2018_ %>% group_by(Entity) %>% slice_max(Year)
ec_13 <- subset(updated_Poverty_rate_50_of_median_LIS_Key_Figures_2018_, select=-c(Year))



ec_hold1 <- merge(ec_1, ec_2, by = "Entity", all = TRUE)
ec_hold2 <- merge(ec_3, ec_4, by = "Entity", all = TRUE)
ec_hold3 <- merge(ec_5, ec_6, by = "Entity", all = TRUE)
ec_hold4 <- merge(ec_7, ec_8, by = "Entity", all = TRUE)
ec_hold5 <- merge(ec_9, ec_10, by = "Entity", all = TRUE)
ec_hold6 <- merge(ec_11, ec_12, by = "Entity", all = TRUE)

ec_hh_1 <- merge(ec_hold1, ec_hold2, by = "Entity", all = TRUE)
ec_hh_2 <- merge(ec_hold3, ec_hold4, by = "Entity", all = TRUE)
ec_hh_3 <- merge(ec_hold5, ec_hold6, by = "Entity", all = TRUE)

ec_hh_1_1 <- merge(ec_hh_1, ec_hh_2, by = "Entity", all = TRUE)
ec_hh_1_2 <- merge(ec_hh_3, ec_13, by = "Entity", all = TRUE)

economic_df <- merge(ec_hh_1_1, ec_hh_1_2, by = "Entity", all = TRUE)

# Healthcare Data

Attitudes_to_Vaccines_Wellcome_Trust_2019_ <- read_csv("Working_Data/healthcare/Attitudes to Vaccines - Wellcome Trust (2019).csv")
updated_Attitudes_to_Vaccines_Wellcome_Trust_2019_ <- Attitudes_to_Vaccines_Wellcome_Trust_2019_ %>% group_by(Entity) %>% slice_max(Year)
hc_1 <- subset(updated_Attitudes_to_Vaccines_Wellcome_Trust_2019_, select=-c(Year))

Clark_Flèche_Senik_Happiness_Inequality <- read_csv("Working_Data/healthcare/Clark, Flèche & Senik – Happiness Inequality.csv")
updated_Clark_Flèche_Senik_Happiness_Inequality <- Clark_Flèche_Senik_Happiness_Inequality %>% group_by(Entity) %>% slice_max(Year)
hc_2 <- subset(updated_Clark_Flèche_Senik_Happiness_Inequality, select=-c(Year))

Disability_Adjusted_Life_Years_WHO_2015_ <- read_csv("Working_Data/healthcare/Disability Adjusted Life Years - WHO (2015).csv")
updated_Disability_Adjusted_Life_Years_WHO_2015_ <- Disability_Adjusted_Life_Years_WHO_2015_ %>% group_by(Entity) %>% slice_max(Year)
hc_3 <- subset(updated_Disability_Adjusted_Life_Years_WHO_2015_, select=-c(Year))

Health_Coverage_ILO_2014_ <- read_csv("Working_Data/healthcare/Health Coverage – ILO (2014).csv")
updated_Health_Coverage_ILO_2014_ <- Health_Coverage_ILO_2014_ %>% group_by(Entity) %>% slice_max(Year)
hc_4 <- subset(updated_Health_Coverage_ILO_2014_, select=-c(Year))

Health_Expenditure_Tanzi_Schuknecht_2000_ <- read_csv("Working_Data/healthcare/Health Expenditure - Tanzi & Schuknecht (2000).csv")
updated_Health_Expenditure_Tanzi_Schuknecht_2000_ <- Health_Expenditure_Tanzi_Schuknecht_2000_ %>% group_by(Entity) %>% slice_max(Year)
hc_5 <- subset(updated_Health_Expenditure_Tanzi_Schuknecht_2000_, select=-c(Year))


Health_Expenditure_and_Financing_OECDstat_2017_ <- read_csv("Working_Data/healthcare/Health Expenditure and Financing - OECDstat (2017).csv")
updated_Health_Expenditure_and_Financing_OECDstat_2017_ <- Health_Expenditure_and_Financing_OECDstat_2017_ %>% group_by(Entity) %>% slice_max(Year)
hc_6 <- subset(updated_Health_Expenditure_and_Financing_OECDstat_2017_, select=-c(Year))

Health_expenditure_per_capita_World_Bank_WDI_2018_ <- read_csv("Working_Data/healthcare/Health expenditure per capita - World Bank WDI (2018).csv")
updated_Health_expenditure_per_capita_World_Bank_WDI_2018_ <- Health_expenditure_per_capita_World_Bank_WDI_2018_ %>% group_by(Entity) %>% slice_max(Year)
hc_7 <- subset(updated_Health_expenditure_per_capita_World_Bank_WDI_2018_, select=-c(Year))

Health_expenditure_per_capita_PPP_World_Bank_2016_ <- read_csv("Working_Data/healthcare/Health expenditure per capita, PPP - World Bank (2016).csv")
updated_Health_expenditure_per_capita_PPP_World_Bank_2016_ <- Health_expenditure_per_capita_PPP_World_Bank_2016_ %>% group_by(Entity) %>% slice_max(Year)
hc_8 <- subset(updated_Health_expenditure_per_capita_PPP_World_Bank_2016_, select=-c(Year))

Healthcare_Access_and_Quality_Index_IHME_2017_ <- read_csv("Working_Data/healthcare/Healthcare Access and Quality Index – IHME (2017).csv")
updated_Healthcare_Access_and_Quality_Index_IHME_2017_ <- Healthcare_Access_and_Quality_Index_IHME_2017_ %>% group_by(Entity) %>% slice_max(Year)
hc_9 <- subset(updated_Healthcare_Access_and_Quality_Index_IHME_2017_, select=-c(Year))

Healthcare_capacity_OECD_2020_ <- read_csv("Working_Data/healthcare/Healthcare capacity (OECD, 2020).csv")
updated_Healthcare_capacity_OECD_2020_ <- Healthcare_capacity_OECD_2020_ %>% group_by(Entity) %>% slice_max(Year)
hc_10 <- subset(updated_Healthcare_capacity_OECD_2020_, select=-c(Year))

Healthy_Life_Expectancy_IHME <- read_csv("Working_Data/healthcare/Healthy Life Expectancy - IHME.csv")
updated_Healthy_Life_Expectancy_IHME <- Healthy_Life_Expectancy_IHME %>% group_by(Entity) %>% slice_max(Year)
hc_11 <- subset(updated_Healthy_Life_Expectancy_IHME, select=-c(Year))

Long_run_series_of_health_expenditure_World_Bank_WDI_2017_ <- read_csv("Working_Data/healthcare/Long-run series of health expenditure - World Bank (WDI) (2017).csv")
updated_Long_run_series_of_health_expenditure_World_Bank_WDI_2017_ <- Long_run_series_of_health_expenditure_World_Bank_WDI_2017_ %>% group_by(Entity) %>% slice_max(Year)
hc_12 <- subset(updated_Long_run_series_of_health_expenditure_World_Bank_WDI_2017_, select=-c(Year))

OECD_Social_Spending_Health <- read_csv("Working_Data/healthcare/OECD Social Spending- Health.csv")
updated_OECD_Social_Spending_Health <- OECD_Social_Spending_Health %>% group_by(Entity) %>% slice_max(Year)
hc_13 <- subset(updated_OECD_Social_Spending_Health, select=-c(Year))

OECD_Social_Spending_Old_Age <- read_csv("Working_Data/healthcare/OECD Social Spending- Old Age.csv")
updated_OECD_Social_Spending_Old_Age <- OECD_Social_Spending_Old_Age %>% group_by(Entity) %>% slice_max(Year)
hc_14 <- subset(updated_OECD_Social_Spending_Old_Age, select=-c(Year))

Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure <- read_csv("Working_Data/healthcare/Out-of-pocket expenditure per capita on healthcare - WHO Global Health Expenditure.csv")
updated_Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure <- Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure %>% group_by(Entity) %>% slice_max(Year)
hc_15 <- subset(updated_Out_of_pocket_expenditure_per_capita_on_healthcare_WHO_Global_Health_Expenditure, select=-c(Year))

Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo <- read_csv("Working_Data/healthcare/Percentage of persons without health insurance - Council of Economic Advisers and National Center fo.csv")
updated_Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo <- Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo %>% group_by(Entity) %>% slice_max(Year)
hc_16 <- subset(updated_Percentage_of_persons_without_health_insurance_Council_of_Economic_Advisers_and_National_Center_fo, select=-c(Year))

Perceptions_of_spending_on_health_expenditure_IPSOS_2016_ <- read_csv("Working_Data/healthcare/Perceptions of spending on health expenditure - IPSOS (2016).csv")
updated_Perceptions_of_spending_on_health_expenditure_IPSOS_2016_ <- Perceptions_of_spending_on_health_expenditure_IPSOS_2016_ %>% group_by(Entity) %>% slice_max(Year)
hc_17 <- subset(updated_Perceptions_of_spending_on_health_expenditure_IPSOS_2016_, select=-c(Year))

Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates <- read_csv("Working_Data/healthcare/Public expenditure on health %GDP – OWID based on WHO and historical estimates.csv")
updated_Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates <- Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates %>% group_by(Entity) %>% slice_max(Year)
hc_18 <- subset(updated_Public_expenditure_on_health_GDP_OWID_based_on_WHO_and_historical_estimates, select=-c(Year))

Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD <- read_csv("Working_Data/healthcare/Total gross official disbursements for medical research and basic heath sectors - OECD.csv")
updated_Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD <- Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD %>% group_by(Entity) %>% slice_max(Year)
hc_19 <- subset(updated_Total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors_OECD, select=-c(Year))

Vaccine_Coverage_and_Disease_Burden_WHO_2017_ <- read_csv("Working_Data/healthcare/Vaccine Coverage and Disease Burden - WHO (2017).csv")
updated_Vaccine_Coverage_and_Disease_Burden_WHO_2017_ <- Vaccine_Coverage_and_Disease_Burden_WHO_2017_ %>% group_by(Entity) %>% slice_max(Year)
hc_20 <- subset(updated_Vaccine_Coverage_and_Disease_Burden_WHO_2017_, select=-c(Year))


h_hold_1 <- merge(hc_1, hc_2, by = "Entity", all = TRUE)
h_hold_2 <- merge(hc_3, hc_4, by = "Entity", all = TRUE)
h_hold_3 <- merge(hc_5, hc_6, by = "Entity", all = TRUE)
h_hold_4 <- merge(hc_7, hc_8, by = "Entity", all = TRUE)
h_hold_5 <- merge(hc_9, hc_10, by = "Entity", all = TRUE)
h_hold_6 <- merge(hc_11, hc_12, by = "Entity", all = TRUE)
h_hold_7 <- merge(hc_13, hc_14, by = "Entity", all = TRUE)
h_hold_8 <- merge(hc_15, hc_16, by = "Entity", all = TRUE)
h_hold_9 <- merge(hc_17, hc_18, by = "Entity", all = TRUE)
h_hold_10 <- merge(hc_19, hc_20, by = "Entity", all = TRUE)

h_hold_1_1 <- merge(h_hold_1, h_hold_2, by = "Entity", all = TRUE)
h_hold_1_2 <- merge(h_hold_3, h_hold_4, by = "Entity", all = TRUE)
h_hold_1_3 <- merge(h_hold_5, h_hold_6, by = "Entity", all = TRUE)
h_hold_1_4 <- merge(h_hold_7, h_hold_8, by = "Entity", all = TRUE)
h_hold_1_5 <- merge(h_hold_9, h_hold_10, by = "Entity", all = TRUE)

h_hold_1_1_1 <- merge(h_hold_1_1, h_hold_1_2, by = "Entity", all = TRUE)
h_hold_1_1_2 <- merge(h_hold_1_3, h_hold_1_4, by = "Entity", all = TRUE)

h_hold_1 <- merge(h_hold_1_1_1, h_hold_1_1_2, by = "Entity", all = TRUE)

healthcare_df <- merge(h_hold_1, h_hold_1_5, by = "Entity", all = TRUE)

# We now bring the final dataframe together

final_hold_1 <- merge(covid_df, demographic_df, by = "Entity", all = TRUE)
final_hold_2 <- merge(disease_df, environment_df, by = "Entity", all = TRUE)
final_hold_3 <- merge(food_water_df, economic_df, by = "Entity", all = TRUE)
final_hold_4 <- merge(healthcare_df, world_stats, by = "Entity", all = TRUE)

final_hold_1_1 <- merge(final_hold_1, final_hold_2, by = "Entity", all = TRUE)
final_hold_1_2 <- merge(final_hold_3, final_hold_4, by = "Entity", all = TRUE)

fully_merged_df <- merge(final_hold_1_1, final_hold_1_2, by = "Entity", all = TRUE)

# Now remove the non-relevant data i.e. countries

fully_merged_df <- fully_merged_df[-c(2,7,10,13,14,16,19,20,21,23,24,25,39,54,55,
                                      56,59,61,65,66,67,68,69,70,71,72,73,74,79,80,81,83,99,
                                      104,105,106,107,108,109,110,111,112,113,114,115,116,117, 118,119,120,122,126,132,133,134,135,136,
                                      137,138,139,140,141,142,143,151,152,156,176,177,178,179,180,181,182,183,184,185,186,187,188,190,
                                      193, 195, 196, 197,198,200, 203,204,205,224,226,227,228,229,230,231,232,233, 234, 235, 237, 238,
                                      241, 242, 243,248,249,250, 251, 252, 253,254,255,256,272,277,278, 279, 280, 281,282,283,284,285,286,
                                      287,288,294,306,311,312,313,316,317,318, 319,323, 324,325, 327,328, 329,341,342,346,375,376,377,
                                      382,383,384, 385,386,387,388,391,392, 393,394, 395, 396,397,398,399,400,401,404, 405, 406, 407, 408,
                                      409,410,412,413,415,432,443,444,446,447,449,450,451,457,463,464,465,467,468,469,470,471,472,473,474,
                                      475,476,478,479, 480, 481,483,486), ]


write.csv(fully_merged_df,"fully_merged_df.csv", row.names = TRUE)


missing_data_summary <- miss_var_summary(fully_merged_df)
missing_data_summary


nrow(fully_merged_df)



# 4 x 100 missing data.
# For the fiscal measures and emergency investment healthcare - the most 
# recent data for all the countries are NA's so the slice_max function 
# doesn't wortk.

# Same with emergency investment healthcare, and international support. 
# So maybe need to go over these and use them in a slightly different way?

# Same with number confirmed polio cases. Just no reading for the latest year/so
# slice max doesn't really work.







