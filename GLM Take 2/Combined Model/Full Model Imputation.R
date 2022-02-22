# Impute the big dataset

library(readr)
library(missForest)
library(datasets)
library(car)
library(readr)
library("janitor")
library('missForest')
library(glm2)
library(MuMIn)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

combined_all_missing <- read_csv("GLM Take 2//Combined Model//combined_all_missing.csv")
combined_all_missing <- subset(combined_all_missing, select = -1)

combined_all_missing$income_support <- as.factor(combined_all_missing$income_support)
combined_all_missing$debt_relief <- as.factor(combined_all_missing$debt_relief)
combined_all_missing$income_classification_world_bank_2017 <- as.factor(combined_all_missing$income_classification_world_bank_2017)


# We now have a dataframe with no response variable, ready to impute.

pre_imputation_no_response <- subset(combined_all_missing, select = -1)

set.seed(100)
combined_imputed_rf <- missForest(as.data.frame(pre_imputation_no_response))
combined_imputed <- as.data.frame.matrix(combined_imputed_rf$ximp)

# We now want to subset this data, so we have the variables we have choosen to put into the model

combined_imputed_res <- cbind(combined_all_missing[1],combined_imputed)
combined_imputed_res
combined_imputed_res <- subset(combined_imputed_res, select = -c(2:12, 14:16, 18, 21:43, 45, 46, 52, 54, 56, 57, 62, 
                                                                 67:73, 75:77, 80:83, 85, 88, 89, 91:94, 96:102, 104:108, 111:114))


write.csv(combined_imputed_res,"GLM Take 2//Combined Model//combined_imputed_response.csv", row.names = TRUE)


combined_imputed_res$income_support <- as.factor(combined_imputed_res$income_support)
combined_imputed_res$debt_relief <- as.factor(combined_imputed_res$debt_relief)
combined_imputed_res$income_classification_world_bank_2017 <- as.factor(combined_imputed_res$income_classification_world_bank_2017)

# Define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(combined_imputed_res)
expl <- expl[-1]

# We now run the GLM Model Function:
after_drop <- gvif_drop(resp, expl, combined_imputed_res, vif_max=5, glmtype=2, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, combined_imputed_res, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

step_AICc_mod <- step2.glm(resp, after_drop, combined_imputed_res, "AICc", Gamma(link="log"), maxit=100)


summary(step_AICc_mod)

par(mfrow = c(2,2))
plot(step_AICc_mod, main="Automated Model GLM, AICc")

par(mfrow=c(1,1))
plot(fitted(step_AICc_mod), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people)
