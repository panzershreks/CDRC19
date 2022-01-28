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
clean_healthcare <- subset(clean_healthcare, select = -1)
clean_healthcare <- clean_names(clean_healthcare)

# View missing data
summary(clean_healthcare)

missing_table <- miss_var_summary(clean_healthcare, sort_miss = TRUE)
missing_table <- data.frame(missing_table)
missing_table

# View variables with more than 50% of missing data
for (i in 1:78){
  if (missing_table$pct_miss[i] > 50){
    print(missing_table$variable[i]) 
  } 
}


# Dropping data below
# Following have more than 50% of missing data (and another reason for dropping data) 

drop <- c("percentage_of_persons_without_health_insurance_percent", #including share_of_population_covered_by_health_insurance_ilo_2014
          "standard_deviation_of_life_satisfaction", # more than 50% missing data and seems irrelevant
          "public_expenditure_on_health_tanzi_schuktnecht_2000", # including health_expenditure_per_capita_ppp_world_bank_2016
          "hospital_beds_nurse_to_bed_ratio_oecd", # including beds and nurses
          "beds_in_not_for_profit_privately_owned_hospitals_number_oecd",# including beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd
          "nurses_headcount_oecd", # including nurses_per_1_000_population_oecd
          "dentists_headcount_oecd", # including dentists_per_100_000_population_oecd
          "midwives_headcount_oecd", # including midwives_per_1_000_live_births_oecd
          "not_for_profit_privately_owned_hospitals_number_oecd", # including not_for_profit_privately_owned_hospitals_per_million_population_oecd
          "beds_in_for_profit_privately_owned_hospitals_number_oecd", # including beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd
          "for_profit_privately_owned_hospitals_number_oecd", # including for_profit_privately_owned_hospitals_per_million_population_oecd
          "physicians_headcount_oecd", # including physicians_per_1_000_population_oecd
          "publicly_owned_hospitals_number_oecd", # including publicly_owned_hospitals_per_million_population_oecd
          "beds_in_publicly_owned_hospitals_number_oecd", # including beds_in_publicly_owned_hospitals_per_1_000_population_oecd
          "long_term_care_beds_number_oecd", # including long_term_care_beds_per_1_000_population_oecd and long_term_care_beds_per_1_000_population_aged_65_oecd
          "general_hospitals_number_oecd", # including general_hospitals_per_million_population_oecd
          "hospitals_number_oecd", # including hospitals_per_million_population_oecd
          "health_expenditure_and_financing_per_capita_oec_dstat_2017",
          "surgical_specialists_headcount_oecd", # including surgical_specialists_per_1_000_population_oecd
          "health",
          "acute_care_beds_number_oecd", # including acute_care_beds_per_1_000_population_oecd
          "psychiatrists_headcount_oecd", # including psychiatrists_per_1_000_population_oecd
          "psychiatric_care_beds_number_oecd", # including psychiatric_care_beds_per_1_000_population_oecd
          "old_age",
          "how_much_we_think_we_spend_on_health_expenditure_ipsos_2016",
          "how_much_we_actually_spend_on_health_expenditure_ipsos_2016",
          "hospital_beds_number_oecd") # including hospital_beds_per_1_000_population_oecd


### The following data has over 50% of missing data but has been decided to keep ###
# beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd
# nurses_per_1_000_population_oecd
# dentists_per_100_000_population_oecd
# midwives_per_1_000_live_births_oecd
# not_for_profit_privately_owned_hospitals_per_million_population_oecd
# beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd
# for_profit_privately_owned_hospitals_per_million_population_oecd
# physicians_per_1_000_population_oecd
# publicly_owned_hospitals_per_million_population_oecd
# beds_in_publicly_owned_hospitals_per_1_000_population_oecd
# long_term_care_beds_per_1_000_population_oecd
# long_term_care_beds_per_1_000_population_aged_65_oecd
# general_hospitals_per_million_population_oecd
# hospitals_per_million_population_oecd
# surgical_specialists_per_1_000_population_oecd
# acute_care_beds_per_1_000_population_oecd
# psychiatrists_per_1_000_population_oecd
# psychiatric_care_beds_per_1_000_population_oecd
# hospital_beds_per_1_000_population_oecd


# Create 'healthcare_d1' without variables with > 50% missing data + justified reason for removing
healthcare_rf <- clean_healthcare[,!(names(clean_healthcare) %in% drop)]
healthcare_rf <- subset(healthcare_rf, select = -1)

healthcare_rf <- as.matrix(healthcare_rf)
healthcare_rf_temp <- missForest(healthcare_rf, maxiter = 5)

healthcare_rf_completed <- healthcare_rf_temp$ximp

healthcare_rf_completed <- data.frame(healthcare_rf_completed)

# Compute correlation 

corr_data <- healthcare_rf_completed[,2:50]  

healthcare_corr <- vis_cor(corr_data) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

healthcare_corr_df <- round(cor(corr_data),2)

View(healthcare_corr_df)

write.csv(healthcare_corr_df,"healthcare_corr.csv", row.names = TRUE)

healthcare_corr <- read_csv("R_Scripts/EmmaR/healthcare_corr.csv")
View(healthcare_corr)

# Remove any variables with corr = 1
#[32] "healthy_life_expectancy_ihme"                                                            
#[33] "life_expectancy_ihme" 

healthcare_rf_completed <- subset(healthcare_rf_completed, select = -c(32, 33))

# VIF 

healthcare_rf_csv <- write.csv(healthcare_rf_completed,"healthcare_rf_csv", row.names = TRUE)

healthcare_rf_model_full <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                              share_of_people_who_agree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_agree_vaccines_are_effective+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              current_health_expenditure_per_capita_ppp_current_international+
                              health_expenditure_per_capita_ppp_world_bank_2016+
                              haq_index_ihme_2017+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                              acute_care_beds_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              hospital_beds_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              dtp3_immunization_coverage_among_1_year_olds_who_2017+
                              polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                              estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                            data = healthcare_rf_completed)

vif(healthcare_rf_model_full)

#create vector of VIF values
vif_values_full <- vif(healthcare_rf_model_full)

max(vif_values_full)

# current_health_expenditure_per_capita_ppp_current_international removed

healthcare_rf_model1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                                 share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                                 share_of_people_who_agree_vaccines_are_safe+
                                 share_of_people_who_disagree_vaccines_are_safe+
                                 share_of_people_who_agree_vaccines_are_effective+
                                 share_of_people_who_disagree_vaccines_are_effective+
                                 share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                                 all_causes_disability_adjusted_life_years_who_2015+
                                 share_of_population_covered_by_health_insurance_ilo_2014+
                                 health_expenditure_per_capita_ppp_world_bank_2016+
                                 haq_index_ihme_2017+
                                 beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                                 beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                                 beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                                 acute_care_beds_per_1_000_population_oecd+
                                 dentists_per_100_000_population_oecd+
                                 for_profit_privately_owned_hospitals_per_million_population_oecd+
                                 general_hospitals_per_million_population_oecd+
                                 hospitals_per_million_population_oecd+
                                 long_term_care_beds_per_1_000_population_oecd+
                                 long_term_care_beds_per_1_000_population_aged_65_oecd+
                                 midwives_per_1_000_live_births_oecd+
                                 not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                                 nurses_per_1_000_population_oecd+
                                 physicians_per_1_000_population_oecd+
                                 psychiatric_care_beds_per_1_000_population_oecd+
                                 psychiatrists_per_1_000_population_oecd+
                                 publicly_owned_hospitals_per_million_population_oecd+
                                 surgical_specialists_per_1_000_population_oecd+
                                 hospital_beds_per_1_000_population_oecd+
                                 out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                                 public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                                 total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                                 bcg_immunization_coverage_among_1_year_olds_who_2017+
                                 hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                                 dtp3_immunization_coverage_among_1_year_olds_who_2017+
                                 polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                                 measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                                 number_of_confirmed_tetanus_cases_who_2017+
                                 number_confirmed_polio_cases_who_2017+
                                 number_of_confirmed_pertussis_cases_who_2017+
                                 number_of_confirmed_measles_cases_who_2017+
                                 number_of_confirmed_diphtheria_cases_who_2017+
                                 estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                                 estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                               data = healthcare_rf_completed)

vif(healthcare_rf_model1)

#create vector of VIF values
vif_values1 <- vif(healthcare_rf_model1)

max(vif_values1)

# dtp3_immunization_coverage_among_1_year_olds_who_2017 removed

healthcare_rf_model2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                             share_of_people_who_agree_vaccines_are_safe+
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model2)

#create vector of VIF values
vif_values2 <- vif(healthcare_rf_model2)

max(vif_values2)

# long_term_care_beds_per_1_000_population_oecd removed 

healthcare_rf_model3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                             share_of_people_who_agree_vaccines_are_safe+
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model3)

#create vector of VIF values
vif_values3 <- vif(healthcare_rf_model3)

max(vif_values3)

# share_of_people_who_agree_vaccines_are_safe removed  

healthcare_rf_model4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model4)

#create vector of VIF values
vif_values4 <- vif(healthcare_rf_model4)

max(vif_values4)

# hospitals_per_million_population_oecd removed

healthcare_rf_model5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model5)

#create vector of VIF values
vif_values5 <- vif(healthcare_rf_model5)

max(vif_values5)

# share_of_people_who_agree_vaccines_are_important_for_children_to_have removed 

healthcare_rf_model6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model6)

#create vector of VIF values
vif_values6 <- vif(healthcare_rf_model6)

max(vif_values6)

# polio_pol3_immunization_coverage_among_1_year_olds_who_2017 removed

healthcare_rf_model7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_publicly_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model7)

#create vector of VIF values
vif_values7 <- vif(healthcare_rf_model7)

max(vif_values7)

# beds_in_publicly_owned_hospitals_per_1_000_population_oecd removed

healthcare_rf_model8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model8)

#create vector of VIF values
vif_values8 <- vif(healthcare_rf_model8)

max(vif_values8)

# haq_index_ihme_2017 removed

healthcare_rf_model9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             hospital_beds_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model9)

#create vector of VIF values
vif_values9 <- vif(healthcare_rf_model9)

max(vif_values9)

# hospital_beds_per_1_000_population_oecd removed 

healthcare_rf_model10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                             acute_care_beds_per_1_000_population_oecd+
                             dentists_per_100_000_population_oecd+
                             for_profit_privately_owned_hospitals_per_million_population_oecd+
                             general_hospitals_per_million_population_oecd+
                             long_term_care_beds_per_1_000_population_aged_65_oecd+
                             midwives_per_1_000_live_births_oecd+
                             not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                             nurses_per_1_000_population_oecd+
                             physicians_per_1_000_population_oecd+
                             psychiatric_care_beds_per_1_000_population_oecd+
                             psychiatrists_per_1_000_population_oecd+
                             publicly_owned_hospitals_per_million_population_oecd+
                             surgical_specialists_per_1_000_population_oecd+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                           data = healthcare_rf_completed)

vif(healthcare_rf_model10)

#create vector of VIF values
vif_values10 <- vif(healthcare_rf_model10)

max(vif_values10)

# health_expenditure_per_capita_ppp_world_bank_2016 removed

healthcare_rf_model11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_agree_vaccines_are_effective+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              acute_care_beds_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                              estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                            data = healthcare_rf_completed)

vif(healthcare_rf_model11)

#create vector of VIF values
vif_values11 <- vif(healthcare_rf_model11)

max(vif_values11)

# share_of_people_who_agree_vaccines_are_effective removed

healthcare_rf_model12 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              acute_care_beds_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                              estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                            data = healthcare_rf_completed)

vif(healthcare_rf_model12)

#create vector of VIF values
vif_values12 <- vif(healthcare_rf_model12)

max(vif_values12)

# beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd removed

healthcare_rf_model13 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              acute_care_beds_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                              estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
                            data = healthcare_rf_completed)

vif(healthcare_rf_model13)

#create vector of VIF values
vif_values13 <- vif(healthcare_rf_model13)

max(vif_values13)

# estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017 removed

healthcare_rf_model14 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              acute_care_beds_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                            data = healthcare_rf_completed)

vif(healthcare_rf_model14)

#create vector of VIF values
vif_values14 <- vif(healthcare_rf_model14)

max(vif_values14)

# acute_care_beds_per_1_000_population_oecd removed

healthcare_rf_model15 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                            data = healthcare_rf_completed)

vif(healthcare_rf_model15)

#create vector of VIF values
vif_values15 <- vif(healthcare_rf_model15)

max(vif_values15)

# hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017 removed

healthcare_rf_model16 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              general_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                            data = healthcare_rf_completed)

vif(healthcare_rf_model16)

#create vector of VIF values
vif_values16 <- vif(healthcare_rf_model16)

max(vif_values16)

# general_hospitals_per_million_population_oecd removed

healthcare_rf_model17 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd+
                              dentists_per_100_000_population_oecd+
                              for_profit_privately_owned_hospitals_per_million_population_oecd+
                              long_term_care_beds_per_1_000_population_aged_65_oecd+
                              midwives_per_1_000_live_births_oecd+
                              not_for_profit_privately_owned_hospitals_per_million_population_oecd+
                              nurses_per_1_000_population_oecd+
                              physicians_per_1_000_population_oecd+
                              psychiatric_care_beds_per_1_000_population_oecd+
                              psychiatrists_per_1_000_population_oecd+
                              publicly_owned_hospitals_per_million_population_oecd+
                              surgical_specialists_per_1_000_population_oecd+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                            data = healthcare_rf_completed)

vif(healthcare_rf_model17)

#create vector of VIF values
vif_values17 <- vif(healthcare_rf_model17)

max(vif_values17)

# All of the VIF scores in healthcare_rf_model17 < 5 

step_healthcare_rf <- step(healthcare_rf_model17)
summary(healthcare_rf_model17)



