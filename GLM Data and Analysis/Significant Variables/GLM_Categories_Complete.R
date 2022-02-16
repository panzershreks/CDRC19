library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)
library(readr)
library(ggplot2)
library(naniar)
library(visdat)
library(missForest)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

# load data and combine
healthcare_sigvars_completed_GLM <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/healthcare_sigvars_completed_GLM.csv")
healthcare_sigvars_completed_GLM <- subset(healthcare_sigvars_completed_GLM, select = -1)

food_imputed_sig_var <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/food_imputed_sig_var.csv")
food_imputed_sig_var <- subset(food_imputed_sig_var, select = -1)

disease_imputed_sig_var <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/disease_imputed_sig_var.csv")
disease_imputed_sig_var <- subset(disease_imputed_sig_var, select = -1)

glm_enviroment_imputed_data_variables <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/glm_enviroment_imputed_data_variables.csv")
glm_enviroment_imputed_data_variables <- subset(glm_enviroment_imputed_data_variables, select = -1)

glm_covid_imputed_data_variables <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/glm_covid_imputed_data_variables.csv")
glm_covid_imputed_data_variables <- subset(glm_covid_imputed_data_variables, select = -1)

demorgraphic_Completed_significant_glm <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/demorgraphic_Completed_significant_glm.csv")
demorgraphic_Completed_significant_glm <- subset(demorgraphic_Completed_significant_glm, select = -1)

econ_significant_complete <- read_csv("GLM Data and Analysis/Significant Variables/Categories Complete/econ_significant_complete.csv")
econ_significant_complete <- subset(econ_significant_complete, select = -1)

all_categories_complete_GLM <- cbind(healthcare_sigvars_completed_GLM, 
                                     food_imputed_sig_var, 
                                     disease_imputed_sig_var, 
                                     glm_enviroment_imputed_data_variables, 
                                     glm_covid_imputed_data_variables, 
                                     demorgraphic_Completed_significant_glm, 
                                     econ_significant_complete)

all_categories_complete_GLM$debt_relief <- as.factor(all_categories_complete_GLM$debt_relief)
all_categories_complete_GLM$income_support <- as.factor(all_categories_complete_GLM$income_support)

# define resonse and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(all_categories_complete_GLM)
expl <- expl[-1]

# drop variables iteratively with VIF > 5
after_drop <- gvif_drop(resp, expl, all_categories_complete_GLM, vif_max=5, glmtype=2, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, all_categories_complete_GLM, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

# backwards selection with AICc criterion
step_final_model <- step2.glm(resp, after_drop, all_categories_complete_GLM, "AICc", Gamma(link="log"), maxit=100)

summary(step_final_model)

# diagnostic plots
par(mfrow = c(2, 2))
plot(step_final_model, main="GLM_Categories_Complete")

# included variable names in model
all.vars(formula(step_final_model)[-1])




