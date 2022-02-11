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

# ADD ECON 

all_categories_complete_GLM <- cbind(healthcare_sigvars_completed_GLM, 
                                     food_imputed_sig_var, 
                                     disease_imputed_sig_var, 
                                     glm_enviroment_imputed_data_variables, 
                                     glm_covid_imputed_data_variables, 
                                     demorgraphic_Completed_significant_glm)

View(all_categories_complete_GLM)

# GLM 

