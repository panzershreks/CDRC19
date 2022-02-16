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
healthcare_sigvars_missing_GLM <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/healthcare_sigvars_missing_GLM.csv")
healthcare_sigvars_missing_GLM <- subset(healthcare_sigvars_missing_GLM, select = -1)

food_missing_sig_var <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/food_missing_sig_var.csv")
food_missing_sig_var <- subset(food_missing_sig_var, select = -1)

disease_missing_sig_var <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/disease_missing_sig_var.csv")
disease_missing_sig_var <- subset(disease_missing_sig_var, select = -1)

glm_enviroment_clean_data_variables <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/glm_enviroment_clean_data_variables.csv")
glm_enviroment_clean_data_variables <- subset(glm_enviroment_clean_data_variables, select = -1)

glm_covid_clean_data_variables <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/glm_covid_clean_data_variables.csv")
glm_covid_clean_data_variables <- subset(glm_covid_clean_data_variables, select = -1)

demorgraphic_Missing_significant_glm <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/demorgraphic_Missing_significant_glm.csv")
demorgraphic_Missing_significant_glm <- subset(demorgraphic_Missing_significant_glm, select = -1)

econ_significant_miss <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/econ_significant_miss.csv")
econ_significant_miss <- subset(econ_significant_miss, select = -1) 

all_categories_missing_GLM <- cbind(healthcare_sigvars_missing_GLM, 
                                    food_missing_sig_var, 
                                    disease_missing_sig_var, 
                                    glm_enviroment_clean_data_variables, 
                                    glm_covid_clean_data_variables, 
                                    demorgraphic_Missing_significant_glm, 
                                    econ_significant_miss)

all_categories_missing_GLM$debt_relief <- as.factor(all_categories_missing_GLM$debt_relief)
all_categories_missing_GLM$income_support <- as.factor(all_categories_missing_GLM$income_support)

# Random Forest 

set.seed(100)

all_categories_missing_GLM_rf <- missForest(as.data.frame(all_categories_missing_GLM))
all_categories_missing_GLM_imputed <- as.data.frame.matrix(all_categories_missing_GLM_rf$ximp)

all_categories_missing_GLM_imputed$debt_relief <- as.factor(all_categories_missing_GLM_imputed$debt_relief)
all_categories_missing_GLM_imputed$income_support <- as.factor(all_categories_missing_GLM_imputed$income_support)

# define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(all_categories_missing_GLM)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, all_categories_missing_GLM_imputed, vif_max=5, glmtype=2, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, all_categories_missing_GLM_imputed, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

step_final_model <- step2.glm(resp, after_drop, all_categories_missing_GLM_imputed, "AICc", Gamma(link="log"), maxit=1000)

summary(step_final_model)

# diagnostic plots
par(mfrow = c(2, 2))
plot(step_final_model, main="GLM_Categories_Missing")

# included variables in model
all.vars(formula(step_final_model)[-1])




