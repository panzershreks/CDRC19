library("ggplot2")
library(grid)
library(gridExtra)
library(lattice)
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library("mice")
library(UpSetR)
library("janitor")
library(corrplot)
library(gt)
library(tictoc)
library(miceRanger)
library(olsrr)
library(missForest)
library(caret)
library(glm2)
library(MuMIn)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

# import data and basic pre-processing
# convert entities to factors and all other columns to numeric, coerce to NAs
econ_data_full <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_economic.csv")
econ_data_full <- clean_names(econ_data_full)
entity_col <- subset(econ_data_full, select = entity)
econ_data_full <- subset(econ_data_full, select = -c(x1, entity))
econ_data_full[] <- lapply(econ_data_full, function(x) as.numeric(as.character(x)))
econ_data_full$entity <- entity_col$entity
econ_data_full$entity <- as.factor(econ_data_full$entity)
econ_data_full <- econ_data_full %>% relocate(entity)

# remove rows where response variable is NA
clean_econ <- econ_data_full %>% drop_na(total_confirmed_deaths_due_to_covid_19_per_million_people)
entity_col <- subset(clean_econ, select = entity)
clean_econ <- subset(clean_econ, select = -c(entity))
clean_econ$income_classification_world_bank_2017 <- as.factor(clean_econ$income_classification_world_bank_2017)
clean_econ$flag <- as.factor(clean_econ$flag)

# remove response for imputation
covid_deaths <- subset(clean_econ, select=1)
clean_econ_no_res <- subset(clean_econ, select=-1)

# impute
econ_rf <- missForest(data.frame(clean_econ_no_res))
econ_rf_df <- as.data.frame(econ_rf$ximp)
any(is.na(econ_rf_df)) # check that no NAs
write.csv(econ_rf_df, file="GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV/econ_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_econ <- cbind(covid_deaths, econ_rf_df)

# import column names we want to keep
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]

# remove junk vars
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
sig_econ_imputed <- subset(full_imputed_econ, select=sig_vars)
sig_econ_missing <- subset(clean_econ, select=sig_vars)
write.csv(sig_econ_imputed, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_econ_imputed.csv", row.names=FALSE)
write.csv(sig_econ_missing, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_econ_missing.csv", row.names=FALSE)















