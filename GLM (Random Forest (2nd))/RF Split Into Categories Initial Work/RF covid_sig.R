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

non_deaths <- c("school_closures","workplace_closures","cancel_public_events",
                "close_public_transport","public_information_campaigns",
                "restrictions_internal_movements","international_travel_controls",
                "fiscal_measures","emergency_investment_healthcare","investment_vaccines",
                "contact_tracing","stringency_index","restriction_gatherings",
                "stay_home_requirements","income_support","debt_relief",
                "international_support","testing_policy","containment_index",
                "facial_coverings","vaccination_policy","effectiveness_larson_et_al_2016",
                "population_with_access_to_improved_sanitation_x",
                "population_without_access_to_improved_sanitation_x",
                "daily_icu_occupancy","daily_icu_occupancy_per_million",
                "daily_hospital_occupancy","daily_hospital_occupancy_per_million",
                "weekly_new_icu_admissions_per_million","weekly_new_hospital_admissions_per_million",
                "importance_larson_et_al_2016","safety_larson_et_al_2016")
covid_deaths <- subset(glm_clean_covid, select=c("total_confirmed_deaths_due_to_covid_19_per_million_people"))
glm_clean_covid <- subset(glm_clean_covid, select=non_deaths)

glm_clean_covid$school_closures <- as.factor(glm_clean_covid$school_closures)
glm_clean_covid$workplace_closures <- as.factor(glm_clean_covid$workplace_closures)
glm_clean_covid$cancel_public_events <- as.factor(glm_clean_covid$cancel_public_events)
glm_clean_covid$close_public_transport <- as.factor(glm_clean_covid$close_public_transport)
glm_clean_covid$public_information_campaigns <- as.factor(glm_clean_covid$public_information_campaigns)
glm_clean_covid$restrictions_internal_movements <- as.factor(glm_clean_covid$restrictions_internal_movements)
glm_clean_covid$international_travel_controls <- as.factor(glm_clean_covid$international_travel_controls)
glm_clean_covid$fiscal_measures <- as.factor(glm_clean_covid$fiscal_measures)
glm_clean_covid$emergency_investment_healthcare <- as.factor(glm_clean_covid$emergency_investment_healthcare)
glm_clean_covid$investment_vaccines <- as.factor(glm_clean_covid$investment_vaccines)
glm_clean_covid$contact_tracing <- as.factor(glm_clean_covid$contact_tracing)
glm_clean_covid$restriction_gatherings <- as.factor(glm_clean_covid$restriction_gatherings)
glm_clean_covid$stay_home_requirements <- as.factor(glm_clean_covid$stay_home_requirements)
glm_clean_covid$international_support <- as.factor(glm_clean_covid$international_support)
glm_clean_covid$testing_policy <- as.factor(glm_clean_covid$testing_policy)
glm_clean_covid$facial_coverings <- as.factor(glm_clean_covid$facial_coverings)
glm_clean_covid$vaccination_policy <- as.factor(glm_clean_covid$vaccination_policy)

# impute

total_confirmed_deaths_due_to_covid_19_per_million_people <- covid_deaths$total_confirmed_deaths_due_to_covid_19_per_million_people
rf_predictrors <- glm_clean_covid


# We impute the missing data using Random Forest
set.seed(100)
covid_rf_df <- rfImpute(x = as.data.frame(rf_predictrors), y = total_confirmed_deaths_due_to_covid_19_per_million_people)
any(is.na(covid_rf_df)) # check that no NAs
covid_rf_df <- subset(covid_rf_df, select = -1)

write.csv(covid_rf_df, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Imputed Full CSV/RF_covid_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_covid <- covid_rf_df

# remove junk vars
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]
keep_index <- which(colnames(full_imputed_covid) %in% keep_cols)
subset_covid <- subset(full_imputed_covid, select=keep_index)

# drop by VIF
resp <- colnames(subset_covid)[1]
expl <- colnames(subset_covid)[-1]
after_drop <- gvif_drop(resp, expl, subset_covid, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_covid, family=Gamma(link="log"))
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, subset_covid, "AICc", Gamma(link="log"), maxit=100)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), subset_covid$total_confirmed_deaths_due_to_covid_19_per_million_people)

# save CSVs
sig_vars <- all.vars(formula(step_drop_vif)[-1])
sig_covid_imputed <- subset(full_imputed_covid, select=sig_vars)
sig_covid_missing <- subset(glm_clean_covid, select=sig_vars)
write.csv(sig_covid_imputed, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Imputed CSV/RF_sig_covid_imputed.csv", row.names=FALSE)
write.csv(sig_covid_missing, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Miss CSV/RF_sig_covid_missing.csv", row.names=FALSE)
