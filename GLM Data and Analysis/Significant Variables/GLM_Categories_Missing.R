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

# ADD ECON 

all_categories_missing_GLM <- cbind(healthcare_sigvars_missing_GLM, 
                                    food_missing_sig_var, 
                                    disease_missing_sig_var, 
                                    glm_enviroment_clean_data_variables, 
                                    glm_covid_clean_data_variables, 
                                    demorgraphic_Missing_significant_glm)

View(all_categories_missing_GLM)

# Random Forest 

# GLM 




