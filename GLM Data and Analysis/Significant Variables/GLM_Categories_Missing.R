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

econ_significant_miss <- read_csv("GLM Data and Analysis/Significant Variables/Categories with Missing/econ_significant_miss.csv")
econ_significant_miss <- subset(econ_significant_miss, select = -1) 


all_categories_missing_GLM <- cbind(healthcare_sigvars_missing_GLM, 
                                    food_missing_sig_var, 
                                    disease_missing_sig_var, 
                                    glm_enviroment_clean_data_variables, 
                                    glm_covid_clean_data_variables, 
                                    demorgraphic_Missing_significant_glm, 
                                    econ_significant_miss)


# Random Forest 

set.seed(100)

all_categories_missing_GLM_rf <- missForest(as.matrix(all_categories_missing_GLM))
all_categories_missing_GLM_imputed <- as.data.frame.matrix(all_categories_missing_GLM_rf$ximp)

# We now run the GLM Model Function:

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=7) {
  gvif_max <- vif_max ^ 0.5
  lm_formula <- lm_formula_paster(resp_var, expl_var)
  model <- glm(lm_formula, data, family = Gamma(link ="log"))
  vif_mod <- vif(model)
  try(gvif <- vif_mod, silent = TRUE)
  try(gvif <- vif_mod[,3], silent = TRUE)
  if (is.null(dim(vif_mod))) {
    gvif_max <- vif_max
  }
  while (max(gvif) > gvif_max) {
    expl_var <- expl_var[-(which.max(gvif))]
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- glm(lm_formula, data, family = Gamma(link ="log"))
    vif_mod <- vif(model)
    try(gvif <- vif_mod, silent = TRUE)
    try(gvif <- vif_mod[,3], silent = TRUE)
    if (is.null(dim(vif_mod))) {
      gvif_max <- vif_max
    }
  }
  return (expl_var)
}

#' create a lm formula from list of variables
#' helper function for gvif_drop
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @return str of formula using the variables provided
lm_formula_paster <- function(resp_var, expl_var) {
  form <- paste0(resp_var, "~")
  for (var in head(expl_var, -1)) {
    form <- paste0(form, var, "+")
  }
  form <- paste0(form, tail(expl_var, 1))
  return (form)
}


resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(all_categories_missing_GLM)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, all_categories_missing_GLM)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, combined_imputed, family = Gamma(link = "log"))
vif(final_model)

after_drop

step_final_model <- step(final_model)

summary(step_final_model)

plot(step_final_model)






