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

set.seed(100)

glm_clean_enviroment <- read.csv("Combined DataFrame Work/CSV Files/Clean/clean_enviroment.csv")
glm_clean_enviroment <- clean_names(glm_clean_enviroment)
glm_clean_enviroment <- subset(glm_clean_enviroment, select = -c(1,2))
glm_clean_enviroment <- glm_clean_enviroment[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                                145, 151, 156, 171,173,177,178, 186,193), ]
glm_clean_enviroment$indoor_5_to_49 <- glm_clean_enviroment$indoor_5_to_9_years + glm_clean_enviroment$indoor_10_to_14_years + glm_clean_enviroment$indoor_15_to_19_years +glm_clean_enviroment$indoor_20_to_24_years + glm_clean_enviroment$indoor_25_to_29_years + glm_clean_enviroment$indoor_30_to_34_years + glm_clean_enviroment$indoor_35_to_39_years + glm_clean_enviroment$indoor_40_to_44_years + glm_clean_enviroment$indoor_45_to_49_years
glm_clean_enviroment$indoor_50_to_79 <- glm_clean_enviroment$indoor_50_to_54_years + glm_clean_enviroment$indoor_55_to_59_years +glm_clean_enviroment$indoor_60_to_64_years + glm_clean_enviroment$indoor_65_to_69_years + glm_clean_enviroment$indoor_70_to_74_years + glm_clean_enviroment$indoor_75_to_79_years 
glm_clean_enviroment$outdoor_5_to_49 <- glm_clean_enviroment$outdoor_5_to_9_years + glm_clean_enviroment$outdoor_10_to_14_years + glm_clean_enviroment$outdoor_15_to_19_years +glm_clean_enviroment$outdoor_20_to_24_years + glm_clean_enviroment$outdoor_25_to_29_years + glm_clean_enviroment$outdoor_30_to_34_years + glm_clean_enviroment$outdoor_35_to_39_years + glm_clean_enviroment$outdoor_40_to_44_years + glm_clean_enviroment$outdoor_45_to_49_years
glm_clean_enviroment$outdoor_50_to_79 <- glm_clean_enviroment$outdoor_50_to_54_years + glm_clean_enviroment$outdoor_55_to_59_years +glm_clean_enviroment$outdoor_60_to_64_years + glm_clean_enviroment$outdoor_65_to_69_years + glm_clean_enviroment$outdoor_70_to_74_years + glm_clean_enviroment$outdoor_75_to_79_years 
drop_4 <- c("indoor_10_to_14_years", "indoor_15_to_19_years", "indoor_20_to_24_years", 
            "indoor_25_to_29_years", "indoor_30_to_34_years", "indoor_35_to_39_years", 
            "indoor_40_to_44_years", "indoor_45_to_49_years", "indoor_5_to_9_years", 
            "indoor_50_to_54_years", "indoor_55_to_59_years", "indoor_60_to_64_years", 
            "indoor_65_to_69_years", "indoor_70_to_74_years", "indoor_75_to_79_years", 
            "outdoor_10_to_14_years", "outdoor_15_to_19_years", "outdoor_20_to_24_years", 
            "outdoor_25_to_29_years", "outdoor_30_to_34_years", "outdoor_35_to_39_years", 
            "outdoor_40_to_44_years", "outdoor_45_to_49_years", "outdoor_5_to_9_years", 
            "outdoor_50_to_54_years", "outdoor_55_to_59_years", "outdoor_60_to_64_years", 
            "outdoor_65_to_69_years", "outdoor_70_to_74_years", "outdoor_75_to_79_years")
glm_clean_enviroment <- glm_clean_enviroment[,!(names(glm_clean_enviroment) %in% drop_4)]

# remove response for imputation
covid_deaths <- subset(glm_clean_enviroment, select=1)
clean_environment_no_res <- subset(glm_clean_enviroment, select=-1)

# impute
environment_rf <- missForest(data.frame(clean_environment_no_res))
environment_rf_df <- as.data.frame(environment_rf$ximp)
any(is.na(environment_rf_df)) # check that no NAs
write.csv(environment_rf_df, file="GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV/environment_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_environment <- cbind(covid_deaths, environment_rf_df)

# import column names we want to keep
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]
keep_cols <- c(keep_cols, "yll_rates_from_all_air_pollution_per_100_000", "death_rates_from_all_air_pollution_per_100_000")

# remove junk vars
keep_index <- which(colnames(full_imputed_environment) %in% keep_cols)
subset_environment <- subset(full_imputed_environment, select=keep_index)

# drop by VIF
resp <- colnames(subset_environment)[1]
expl <- colnames(subset_environment)[-1]
after_drop <- gvif_drop(resp, expl, subset_environment, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_environment, family=Gamma(link="log"))
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, subset_environment, "AICc", Gamma(link="log"), maxit=100)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), subset_environment$total_confirmed_deaths_due_to_covid_19_per_million_people)

# save CSVs
sig_vars <- all.vars(formula(step_drop_vif)[-1])
sig_environment_imputed <- subset(full_imputed_environment, select=sig_vars)
sig_environment_missing <- subset(clean_environment_no_res, select=sig_vars)
write.csv(sig_environment_imputed, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_environment_imputed.csv", row.names=FALSE)
write.csv(sig_environment_missing, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_environment_imputed.csv", row.names=FALSE)


































