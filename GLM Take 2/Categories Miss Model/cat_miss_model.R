library(readr)
library("janitor")
library(missForest)
library(MuMIn)
library(glm2)
library(dplyr)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

covid_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_covid_missing.csv"))
covid_df$income_support <- as.factor(covid_df$income_support)
disease_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_disease_missing.csv"))
econ_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_econ_missing.csv"))
econ_df$income_classification_world_bank_2017 <- as.factor(econ_df$income_classification_world_bank_2017)
environment_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_environment_missing.csv"))
food_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_food_missing.csv"))
healthcare_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_healthcare_missing.csv"))

cat_combined <- cbind(covid_df, disease_df, econ_df, environment_df, food_df, healthcare_df)

# impute
cat_combined_rf <- missForest(data.frame(cat_combined))
cat_combined_rf_df <- as.data.frame(cat_combined_rf$ximp)
any(is.na(cat_combined_rf_df)) # check that no NAs

# add response var
entity_response_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/entity_and_response.csv"))
entity_col <- subset(entity_response_df, select=1)
response_col <- subset(entity_response_df, select=2)
full_df <- cbind(response_col, cat_combined_rf_df)

# drop by VIF
resp <- colnames(full_df)[1]
expl <- colnames(full_df)[-1]
after_drop <- gvif_drop(resp, expl, full_df, vif_max=5, glmtype=2, maxit=1000)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, full_df, family=Gamma(link="log"), maxit=100)
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, full_df, "AICc", Gamma(link="log"), maxit=1000)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people)



































