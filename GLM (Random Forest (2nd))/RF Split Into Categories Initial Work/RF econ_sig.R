# Random Forest Imputation Economic Data

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


clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)

response_variable <- clean_fully_merged[,57]

economic_all_missing <- subset(clean_fully_merged, select = c(238:273))
names(economic_all_missing)[names(economic_all_missing) == 'mean'] <- 'mean_monthly_income'
economic_all_missing$gdp_growth_per_capita_from_previous_year_2020_q2 <- with(economic_all_missing, gdp_growth_from_previous_year_2020_q2 / pop2021)
to_drop <- c("pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
             "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
             "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
             "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y",
             "gdp_per_capita_ppp_2011_wdi_2016", "gdp", "gdp_growth_from_previous_year_2020_q2",
             "pop2021", "gini_coefficient_world_bank_2016", 
             "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017")

economic_all_missing = economic_all_missing[,!(names(economic_all_missing)%in% to_drop)]
economic_all_missing$income_classification_world_bank_2017 <- as.factor(economic_all_missing$income_classification_world_bank_2017)


total_confirmed_deaths_due_to_covid_19_per_million_people <- response_variable$total_confirmed_deaths_due_to_covid_19_per_million_people
rf_predictrors <- economic_all_missing

set.seed(100)
econ_rf_df <- rfImpute(x = as.data.frame(rf_predictrors), y = total_confirmed_deaths_due_to_covid_19_per_million_people)
any(is.na(econ_rf_df)) # check that no NAs
econ_rf_df <- subset(econ_rf_df, select = -1)

write.csv(econ_rf_df, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Imputed Full CSV/RF_econ_full_imputed.csv", row.names=FALSE)

full_imputed_econ <- cbind(response_variable, econ_rf_df)

# remove junk vars
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]
keep_index <- which(colnames(full_imputed_econ) %in% keep_cols)
subset_econ <- subset(full_imputed_econ, select=keep_index)

# drop by VIF
resp <- colnames(subset_econ)[1]
expl <- colnames(subset_econ)[-1]
after_drop <- gvif_drop(resp, expl, subset_econ, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_econ, family=Gamma(link="log"))
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, subset_econ, "AICc", Gamma(link="log"), maxit=100)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), subset_econ$total_confirmed_deaths_due_to_covid_19_per_million_people)

# save CSVs
sig_vars <- all.vars(formula(step_drop_vif)[-1])
sig_econ_imputed <- subset(econ_rf_df, select=sig_vars)
sig_econ_missing <- subset(economic_all_missing, select=sig_vars)
write.csv(sig_econ_imputed, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Imputed CSV/RF_sig_econ_imputed.csv", row.names=FALSE)
write.csv(sig_econ_missing, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Miss CSV/RF_sig_econ_missing.csv", row.names=FALSE)










