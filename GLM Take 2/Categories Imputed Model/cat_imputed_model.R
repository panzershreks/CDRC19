library(readr)
library("janitor")
library(missForest)
library(MuMIn)
library(glm2)
library(dplyr)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

covid_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_covid_imputed.csv"))
covid_df$income_support <- as.factor(covid_df$income_support)
disease_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_disease_imputed.csv"))
econ_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_econ_imputed.csv"))
econ_df$income_classification_world_bank_2017 <- as.factor(econ_df$income_classification_world_bank_2017)
environment_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_environment_imputed.csv"))
food_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_food_imputed.csv"))
healthcare_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_healthcare_imputed.csv"))
demog_df <- data.frame(read_csv("GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_demorgraphic_imputed.csv"))


entity_response_df <- data.frame(read_csv(file="GLM Take 2/Split Into Categories Initial Work/entity_and_response.csv"))
entity_col <- subset(entity_response_df, select=1)
response_col <- subset(entity_response_df, select=2)
full_df <- cbind(response_col, covid_df, disease_df, econ_df, environment_df, food_df, healthcare_df, demog_df)

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
plot(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people) +
  title("Fitted Values and Covid-19 Death Rates")


source("LM Data and Analysis/list_of_new_countries.R")
# code to add country text to the plot,
par(mfrow=c(1,1))
plot(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  text(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people, 
       row.names(list_of_countries), cex=0.6, pos=4, col="red")

source("LM Data and Analysis/list_of_new_countries.R")
# code to add country text to the plot,
par(mfrow=c(1,1))
plot(fitted(step_drop_vif)[c(9,134,34,78,159,4,29,46,97,110)], full_df$total_confirmed_deaths_due_to_covid_19_per_million_people[c(9,134,34,78,159,4,29,46,97,110)]) + 
  text(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people, 
       row.names(list_of_countries), cex=0.6, pos=4, col="red")



print(xtable(step_drop_vif, type = "latex"), file = "All Model Latex/MF_b.tex")















