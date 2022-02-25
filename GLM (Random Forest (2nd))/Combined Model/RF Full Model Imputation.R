# Full Model Imputation with New Random Forest Method

library(randomForest)
library(readr)
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

total_confirmed_deaths_due_to_covid_19_per_million_people <- combined_all_missing$total_confirmed_deaths_due_to_covid_19_per_million_people
rf_predictrors <- combined_all_missing[,-1]


# We impute the missing data using Random Forest

set.seed(100)
combined_imputed <- rfImpute(x = as.data.frame(rf_predictrors), y = total_confirmed_deaths_due_to_covid_19_per_million_people)

combined_imputed_res <- subset(combined_imputed, select = -c(2:12, 14:16, 18, 21:43, 45, 46, 52, 54, 56, 57, 62, 
                                                                 67:73, 75:77, 80:83, 85, 88, 89, 91:94, 96:102, 104:108, 111:114))

write.csv(combined_imputed_res,"GLM (Random Forest (2nd))//Combined Model//rf_combined_imputed.csv", row.names = TRUE)


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
plot(fitted(step_AICc_mod), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  title("Fitted Values and Covid-19 Death Rates")


source("LM Data and Analysis/list_of_new_countries.R")
# code to add country text to the plot,
par(mfrow=c(1,1))
plot(fitted(step_AICc_mod), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  text(fitted(step_AICc_mod), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people, 
       row.names(list_of_countries), cex=0.6, pos=4, col="red")


