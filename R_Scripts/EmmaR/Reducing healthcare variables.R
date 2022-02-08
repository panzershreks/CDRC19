library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)
library(readr)
library(ggplot2)
library(naniar)
library(visdat)
library(missForest)

# Import data 
clean_healthcare <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_healthcare.csv")

# Remove first column 
clean_healthcare <- subset(clean_healthcare, select = -1)

# Remove countries with no death data
clean_healthcare <- clean_healthcare[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                        145, 151, 156, 171,173,177,178, 186,193), ]
clean_healthcare <- clean_names(clean_healthcare)

# Dropping data below
drop <- c("share_of_people_who_disagree_vaccines_are_important_for_children_to_have", 
          "share_of_people_who_agree_vaccines_are_safe", 
          "share_of_people_who_disagree_vaccines_are_safe", 
          "share_of_people_who_agree_vaccines_are_effective",
          "share_of_people_who_disagree_vaccines_are_effective",
          "share_of_people_who_agree_vaccines_are_important_for_children_to_have", 
          "public_expenditure_on_health_tanzi_schuktnecht_2000",
          "health_expenditure_and_financing_per_capita_oec_dstat_2017",
          "health_expenditure_per_capita_ppp_world_bank_2016", 
          "beds_in_for_profit_privately_owned_hospitals_number_oecd", 
          "beds_in_not_for_profit_privately_owned_hospitals_number_oecd",
          "beds_in_publicly_owned_hospitals_number_oecd", 
          "acute_care_beds_number_oecd", 
          "dentists_headcount_oecd", 
          "for_profit_privately_owned_hospitals_number_oecd", 
          "general_hospitals_number_oecd", 
          "hospitals_number_oecd", 
          "long_term_care_beds_number_oecd", 
          "long_term_care_beds_per_1_000_population_aged_65_oecd", 
          "midwives_headcount_oecd", 
          "not_for_profit_privately_owned_hospitals_number_oecd", 
          "nurses_headcount_oecd", 
          "physicians_headcount_oecd", 
          "psychiatric_care_beds_number_oecd", 
          "psychiatrists_headcount_oecd", 
          "publicly_owned_hospitals_number_oecd", 
          "surgical_specialists_headcount_oecd", 
          "hospital_beds_number_oecd", 
          "hospital_beds_nurse_to_bed_ratio_oecd", 
          "health", 
          "old_age",
          "public_expenditure_on_health_per_capita_in_developing_countries_ppp_world_bank_wdi_2017", 
          "how_much_we_think_we_spend_on_health_expenditure_ipsos_2016",
          "how_much_we_actually_spend_on_health_expenditure_ipsos_2016", 
          "public_expenditure_on_health_percent_gdp_owid_extrapolated_series", 
          "bcg_immunization_coverage_among_1_year_olds_who_2017", 
          "hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017",
          "dtp3_immunization_coverage_among_1_year_olds_who_2017", 
          "polio_pol3_immunization_coverage_among_1_year_olds_who_2017", 
          "measles_mcv_immunization_coverage_among_1_year_olds_who_2017", 
          "number_of_confirmed_tetanus_cases_who_2017", 
          "number_confirmed_polio_cases_who_2017", 
          "number_of_confirmed_pertussis_cases_who_2017", 
          "number_of_confirmed_measles_cases_who_2017", 
          "number_of_confirmed_diphtheria_cases_who_2017", 
          "estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017", 
          "estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017", 
          "percentage_of_persons_without_health_insurance_percent",
          "healthy_life_expectancy_ihme",                                                            
          "life_expectancy_ihme")

healthcare_all_mis <- clean_healthcare[,!(names(clean_healthcare) %in% drop)]
healthcare_all_mis <- subset(healthcare_sub, select = -1)

View(healthcare_all_mis)

write.csv(healthcare_all_mis, file = "healthcare_all_mis.csv", row.names = TRUE)




