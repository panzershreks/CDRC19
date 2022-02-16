# GLM Matthew Categories

library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

combined_all_missing <- read_csv("GLM Data and Analysis/Combined CSV/combined_all_missing.csv")
combined_all_missing <- clean_names(combined_all_missing)
combined_all_missing <- subset(combined_all_missing, select = -1)

# now subset disease variables and response
# response = column 1
# disease = 19 -> 46
# food/water = 2 -> 18 + world stats = 47

disease_missing <- subset(combined_all_missing, select = c(1,19:46))

set.seed(100)
disease_rf <- missForest(as.matrix(disease_missing))
disease_imputed <- as.data.frame.matrix(disease_rf$ximp)

# define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(disease_imputed)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, disease_imputed, vif_max=5, glmtype=1, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, disease_imputed, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

step_AICc_mod <- step2.glm(resp, after_drop, disease_imputed, "AICc", Gamma(link="log"), maxit=100)

summary(step_AICc_mod)
all.vars(formula(step_AICc_mod)[-1])
# We now want to take the variables output from the step function and put them into a dataframe.
# First we do this for the imputed data

disease_imputed_sig_var <- subset(disease_imputed, select = c(
  age_standardised_diabetes_prevalence_male,
  ratio_of_diabetes_to_overweight_prevalence,
  deaths_no_access_to_handwashing_facility_sex_both_age_70_number,
  kidney_disease_ihme_2017,
  diabetes_blood_and_endocrine_disease_ihme_2017,
  meningitis_ihme_2017,
  prevalence_of_obesity_female_who_2019
))

write.csv(disease_imputed_sig_var,"GLM Data and Analysis/Significant Variables/Categories Complete/disease_imputed_sig_var.csv", row.names = TRUE)


# Now we do this for the data before random forests


disease_missing_sig_var <- subset(disease_missing, select = c(
  age_standardised_diabetes_prevalence_male,
  ratio_of_diabetes_to_overweight_prevalence,
  deaths_no_access_to_handwashing_facility_sex_both_age_70_number,
  kidney_disease_ihme_2017,
  diabetes_blood_and_endocrine_disease_ihme_2017,
  meningitis_ihme_2017,
  prevalence_of_obesity_female_who_2019
))

write.csv(disease_missing_sig_var,"GLM Data and Analysis/Significant Variables/Categories with Missing/disease_missing_sig_var.csv", row.names = TRUE)









