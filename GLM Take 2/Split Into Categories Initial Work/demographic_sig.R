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

setwd("C:\\Users\\51291\\Documents\\Rstudio")
set.seed(100)

glm_clean_demographic <- read.csv("clean_demographic.csv")
glm_clean_demographic <- clean_names(glm_clean_demographic)
glm_clean_demographic <- subset(glm_clean_demographic, select = -c(1,2))

glm_clean_demographic_new <- glm_clean_demographic[-c(5,13:24)]
demographic_missing_vis <- vis_miss(glm_clean_demographic_new, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))

non_deaths <- colnames(glm_clean_demographic_new)[-1]
glm_clean_cdemographic <- subset(glm_clean_demographic_new, select=non_deaths)
demographic_deaths <- subset(glm_clean_demographic_new, select=c("total_confirmed_deaths_due_to_covid_19_per_million_people"))

# impute
demographic_rf <- missForest(data.frame(glm_clean_cdemographic))
demographic_rf_df <- as.data.frame(demographic_rf$ximp)
any(is.na(demographic_rf_df)) # check that no NAs
write.csv(demographic_rf_df, file="demographic_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_demographic <- cbind(demographic_deaths, demographic_rf_df)

# remove junk vars
keep_cols <- colnames(read_csv(file = "subset_of_total.csv"))[-1]
keep_index <- which(colnames(glm_clean_demographic_new) %in% keep_cols)
subset_covid <- subset(full_imputed_demographic, select=keep_index)

# drop by VIF
resp <- colnames(subset_covid)[1]
expl <- colnames(subset_covid)[-1]
after_drop <- gvif_drop(resp, expl, subset_covid, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_covid, family=Gamma(link="log"))
fitted(model_drop_vif)

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
sig_covid_imputed <- subset(full_imputed_demographic, select=sig_vars)
sig_covid_missing <- subset(glm_clean_demographic, select=sig_vars)
write.csv(sig_covid_imputed, file="sig_covid_imputed.csv", row.names=FALSE)
write.csv(sig_covid_missing, file="sig_covid_missing.csv", row.names=FALSE)
