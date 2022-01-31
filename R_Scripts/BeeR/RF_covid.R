library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(tidyverse)
library(readr)
library(dplyr)
library(naniar)
library(car)
library(visdat)
library(mice)
library(UpSetR)
library(janitor)
library(corrplot)
library(gt)
library(missForest)
library(stargazer)

clean_covid <- read.csv("~/Documents/University/YEAR 4/ALL YEAR/MATHEMATICAL PROJECT/ANALYSIS/CDRC19/Combined DataFrame Work/CSV Files/Clean/clean_covid.csv")
clean_covid <- clean_names(clean_covid)
clean_covid <- subset(clean_covid, select = -c(1,2))
clean_covid <- clean_covid[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                              145, 151, 156, 171,173,177,178, 186,193), ]
drop_2 <- c("weekly_new_icu_admissions",
            "weekly_new_icu_admissions_per_million",
            "daily_icu_occupancy",
            "daily_icu_occupancy_per_million",
            "weekly_new_hospital_admissions",
            "weekly_new_hospital_admissions_per_million",
            "total_covid_19_tests_performed_per_million_people",
            "daily_hospital_occupancy",
            "daily_hospital_occupancy_per_million",
            "total_covid_19_tests_performed",
            "importance_larson_et_al_2016",	
            "safety_larson_et_al_2016",	
            "effectiveness_larson_et_al_2016",
            "alpha",
            "b_1_1_277",
            "b_1_1_302",
            "b_1_160",
            "b_1_177",
            "b_1_221",
            "b_1_258",
            "b_1_367",
            "beta",	
            "delta",	
            "epsilon",	
            "eta",		
            "gamma",	
            "iota",		
            "kappa",	
            "s_677h_robin1",	
            "s_677p_pelican",
            "others",		
            "non_who",
            "lambda",
            "b_1_621",
            "b_1_620",
            "b_1_1_519",
            "emergency_investment_healthcare",
            "investment_vaccines",
            "international_support",
            # take out last three as they are all 0 with a few NAs
            # all sources for stringency index so do not need
            "school_closures",
            "workplace_closures",
            "cancel_public_events",
            "close_public_transport",
            "public_information_campaigns",
            "restrictions_internal_movements",
            "international_travel_controls",
            "contact_tracing",
            "restriction_gatherings",
            "stay_home_requirements",
            "testing_policy",
            "facial_coverings",
            "vaccination_policy",
            "fiscal_measures",
            "has_population_5m_and_had_100_cases_21_days_ago_and_has_testing_data")

rf_clean_covid <- clean_covid[,!(names(clean_covid) %in% drop_2)]

set.seed(100)
covid_rf <- missForest(as.matrix(rf_clean_covid))
covid_data <- covid_rf$ximp
covid_data <- as.data.frame.matrix(covid_data)

write.csv(covid_data, file = "rf_covid_data.csv", row.names = TRUE)

# take out highly correlated variables
# 4 taken out are weekly_cases, weekly_deaths, case_fatality_rate_of_covid_19 and days_since_the_total_confirmed_cases_of_covid_19_reached_100

resp_var_covid <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl_var_covid <- c("stringency_index", "
  income_support", "debt_relief", "containment_index", "
  population_with_access_to_improved_sanitation_x", "
  population_without_access_to_improved_sanitation_x", "
  daily_new_confirmed_cases_of_covid_19", "
  daily_new_confirmed_deaths_due_to_covid_19", "
  total_confirmed_cases_of_covid_19", "
  total_confirmed_deaths_due_to_covid_19", "
  daily_new_confirmed_cases_of_covid_19_per_million_people", "
  daily_new_confirmed_deaths_due_to_covid_19_per_million_people", "
  total_confirmed_cases_of_covid_19_per_million_people", "
  days_since_the_total_confirmed_deaths_of_covid_19_reached_5", "
  days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1", "
  days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1", "case_fatality_rate_of_covid_19_only_observations_with_100_cases", "
  days_since_30_daily_new_confirmed_cases_recorded", "days_since_50_daily_new_confirmed_cases_recorded", "
  days_since_10_daily_new_confirmed_deaths_recorded", "days_since_5_daily_new_confirmed_deaths_recorded", "
  days_since_3_daily_new_confirmed_deaths_recorded", "
  daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned", "
  daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned", "
  days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1", "
  days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1", "
  daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned", "
  daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned", "
  daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned", "
  daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned", "
  days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30", "
  days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5", "
  days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01", "
  daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned", "
  daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned", "
  days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m", "
  doubling_days_of_total_confirmed_cases_3_day_period", "doubling_days_of_total_confirmed_cases_7_day_period", "
  doubling_days_of_total_confirmed_deaths_3_day_period", "doubling_days_of_total_confirmed_deaths_7_day_period", "weekly_case_growth", "weekly_death_growth", "biweekly_cases", "biweekly_deaths", "biweekly_case_growth", "
  biweekly_death_growth", "weekly_cases_per_million_people", "weekly_deaths_per_million_people", "
  biweekly_cases_per_million_people", "biweekly_deaths_per_million_people", "
  case_fatality_rate_of_covid_19_short_term")
after_drop_covid <- gvif_drop(resp_var_covid, expl_var_covid, covid_data)
final_formula_covid <- lm_formula_paster(resp_var_covid, after_drop_covid)
final_model_covid <- lm(final_formula_covid, covid_data)
vif(final_model_covid)
stargazer(vif(final_model_covid), type='latex', summary=FALSE)
step(final_model_covid)
summary(final_model_covid)
plot(final_model_covid)

#Call: without the 4 very correlated ones removed
# take out highly correlated variables
# 4 taken out are weekly_cases, weekly_deaths, case_fatality_rate_of_covid_19 and days_since_the_total_confirmed_cases_of_covid_19_reached_100
#lm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#     stringency_index + debt_relief + containment_index + population_with_access_to_improved_sanitation_x + 
#     total_confirmed_deaths_due_to_covid_19 + daily_new_confirmed_cases_of_covid_19_per_million_people + 
#     daily_new_confirmed_deaths_due_to_covid_19_per_million_people + 
#     total_confirmed_cases_of_covid_19_per_million_people + 
#     days_since_the_total_confirmed_cases_of_covid_19_reached_100 + 
#     case_fatality_rate_of_covid_19 + days_since_30_daily_new_confirmed_cases_recorded + 
#    days_since_10_daily_new_confirmed_deaths_recorded + daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned + 
#    days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01 + 
#    doubling_days_of_total_confirmed_deaths_3_day_period + 
#    weekly_case_growth + weekly_cases_per_million_people + 
#    biweekly_deaths_per_million_people + case_fatality_rate_of_covid_19_short_term, 
#  data = covid_data)

#lm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#debt_relief + population_without_access_to_improved_sanitation_x + 
#  total_confirmed_deaths_due_to_covid_19 + daily_new_confirmed_cases_of_covid_19_per_million_people + 
#  total_confirmed_cases_of_covid_19_per_million_people + 
#  case_fatality_rate_of_covid_19_only_observations_with_100_cases + 
#  days_since_10_daily_new_confirmed_deaths_recorded + weekly_case_growth + 
#  weekly_death_growth + biweekly_deaths_per_million_people + 
#  case_fatality_rate_of_covid_19_short_term, data = covid_data)

#final_model_after_step <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#                               debt_relief + population_without_access_to_improved_sanitation_x + 
#                               total_confirmed_deaths_due_to_covid_19 + daily_new_confirmed_cases_of_covid_19_per_million_people + 
#                               total_confirmed_cases_of_covid_19_per_million_people + 
#                               case_fatality_rate_of_covid_19_only_observations_with_100_cases + 
#                               days_since_10_daily_new_confirmed_deaths_recorded + weekly_case_growth + 
#                               weekly_death_growth + biweekly_deaths_per_million_people + 
#                               case_fatality_rate_of_covid_19_short_term, data = covid_data)
