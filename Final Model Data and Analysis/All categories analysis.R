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

## Combining dataframes that are imputed BEFORE collation 

healthcare_sigvars_completed <- read_csv("Final Model Data and Analysis/Categories Complete/healthcare_sigvars_completed.csv")
healthcare_sigvars_completed <- subset(healthcare_sigvars_completed, select = -1)

matthew_sig_var_complete <- read_csv("Final Model Data and Analysis/Categories Complete/matthew_sig_var_complete.csv")
matthew_sig_var_complete <- subset(matthew_sig_var_complete, select = -1)

rf_covid_imputed_data_variables <- read_csv("Final Model Data and Analysis/Categories Complete/rf_covid_imputed_data_variables.csv")
rf_covid_imputed_data_variables <- subset(rf_covid_imputed_data_variables, select = -1)

rf_enviroment_imputed_data_variables <- read_csv("Final Model Data and Analysis/Categories Complete/rf_enviroment_imputed_data_variables.csv")
rf_enviroment_imputed_data_variables <- subset(rf_enviroment_imputed_data_variables, select = -1)

econ_significant <- read_csv("Final Model Data and Analysis/Categories Complete/econ_significant.csv")
econ_significant <- subset(econ_significant, select = -1)

# NEED TO CHANGE THIS POTENTIALLY 
Without_missing_data_demographic_ <- read_csv("Final Model Data and Analysis/Categories Complete/Without_missing data(demographic).csv")
Without_missing_data_demographic_ <- subset(Without_missing_data_demographic_, select = -1)

# NEED TO ADD DEMOGRAPHIC TO THIS 
all_categories_complete <- cbind(matthew_sig_var_complete, healthcare_sigvars_completed, rf_covid_imputed_data_variables, 
                                 rf_enviroment_imputed_data_variables, econ_significant)
  
View(all_categories_complete)

# Automated VIF function 

# Using score threshold of 5 

gvif_drop <- function(resp_var, expl_var, data, vif_max=5) {
  gvif_max <- vif_max ^ 0.5
  lm_formula <- lm_formula_paster(resp_var, expl_var)
  model <- lm(lm_formula, data)
  vif_mod <- vif(model)
  try(gvif <- vif_mod, silent = TRUE)
  try(gvif <- vif_mod[,3], silent = TRUE)
  if (is.null(dim(vif_mod))) {
    gvif_max <- vif_max
  }
  while (max(gvif) > gvif_max) {
    expl_var <- expl_var[-(which.max(gvif))]
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- lm(lm_formula, data)
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

# NEED TO ADD THIS 
expl <- c()

after_drop <- gvif_drop(resp, expl, all_categories_complete)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, all_categories_complete)
vif(final_model)

step_all_categories <- step(final_model)
summary(step_all_categories)

# We now plot our model assumptions:

par(mfrow = c(2, 2))
plot(step_all_categories)



## Combining missing dataframes & random forest imputation 

