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

covid_all_missing <- read_csv("GLM Data and Analysis/Category CSV/covid_all_missing.csv")
covid_all_missing <- clean_names(covid_all_missing)
covid_all_missing <- subset(covid_all_missing, select = -1)
covid_all_missing$income_support <- as.factor(covid_all_missing$income_support)
glm_clean_covid <- subset(covid_all_missing, select=-debt_relief)


# impute

total_confirmed_deaths_due_to_covid_19_per_million_people <- response_variable$total_confirmed_deaths_due_to_covid_19_per_million_people
rf_predictrors <- glm_clean_covid


# We impute the missing data using Random Forest
set.seed(100)
covid_rf_df <- rfImpute(x = as.data.frame(rf_predictrors), y = total_confirmed_deaths_due_to_covid_19_per_million_people)
any(is.na(covid_rf_df)) # check that no NAs
covid_rf_df <- subset(covid_rf_df, select = -1)

write.csv(covid_rf_df, file="GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Imputed Full CSV/RF_covid_full_imputed.csv", row.names=FALSE)

full_imputed_covid <- cbind(response_variable, covid_rf_df)

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
