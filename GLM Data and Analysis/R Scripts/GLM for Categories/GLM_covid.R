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

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

glm_clean_covid <- read.csv("Combined DataFrame Work/CSV Files/Clean/clean_covid.csv")
glm_clean_covid <- clean_names(glm_clean_covid)
glm_clean_covid <- subset(glm_clean_covid, select = -c(1,2))
glm_clean_covid$income_support <- as.factor(glm_clean_covid$income_support)
glm_clean_covid$debt_relief <- as.factor(glm_clean_covid$debt_relief)
glm_clean_covid <- glm_clean_covid[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                              145, 151, 156, 171,173,177,178, 186,193), ]
covid_missing_vis <- vis_miss(glm_clean_covid, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
covid_missing_vis
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
            "population_with_access_to_improved_sanitation_x")

glm_clean_covid <- glm_clean_covid[,!(names(glm_clean_covid) %in% drop_2)]

set.seed(100)
glm_covid_rf <- missForest(data.frame(glm_clean_covid))
glm_covid_data <- glm_covid_rf$ximp

# define response and explanatory variables
resp_var_covid <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl_var_covid <- c("stringency_index", "income_support", "debt_relief", "containment_index")
after_drop_covid <- gvif_drop(resp_var_covid, expl_var_covid, glm_covid_data, vif_max = 5)
final_formula_covid <- lm_formula_paster(resp_var_covid, after_drop_covid)
final_model_covid <- glm(final_formula_covid, glm_covid_data, family = Gamma(link = "log"))
vif(final_model_covid)

step_final_model_covid <- step2.glm(resp_var_covid, after_drop_covid, glm_covid_data, "AICc", Gamma(link="log"), maxit=100)
summary(step_final_model_covid)
par(mfrow = c(2, 2))
plot(step_final_model_covid)




glm_covid_imputed_data_variables <- subset(glm_covid_data, select = c("income_support", "stringency_index", "debt_relief"))
write.csv(glm_covid_imputed_data_variables,
          file = "GLM Data and Analysis/Significant Variables/Categories Complete/glm_covid_imputed_data_variables.csv",
          row.names = TRUE)

# now another dataset with the significant variables from the data before it was imputed

glm_covid_clean_data_variables <- subset(glm_clean_covid, select = c("income_support", "stringency_index", "debt_relief"))
write.csv(glm_covid_clean_data_variables,
          file = "GLM Data and Analysis/Significant Variables/Categories with Missing/glm_covid_clean_data_variables.csv",
          row.names = TRUE)




