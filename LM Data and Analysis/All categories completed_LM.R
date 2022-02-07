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

healthcare_sigvars_completed <- read_csv("LM Data and Analysis/Categories Complete/healthcare_sigvars_completed.csv")
healthcare_sigvars_completed <- subset(healthcare_sigvars_completed, select = -1)

matthew_sig_var_complete <- read_csv("LM Data and Analysis/Categories Complete/matthew_sig_var_complete.csv")
matthew_sig_var_complete <- subset(matthew_sig_var_complete, select = -1)

enviroment_imputed_data_variables <- read_csv("LM Data and Analysis/Categories Complete/enviroment_imputed_data_variables.csv")
enviroment_imputed_data_variables <- subset(enviroment_imputed_data_variables, select = -1)

covid_imputed_data_variables <- read_csv("LM Data and Analysis/Categories Complete/covid_imputed_data_variables.csv")
covid_imputed_data_variables <- subset(covid_imputed_data_variables, select = -1)

econ_significant <- read_csv("LM Data and Analysis/Categories Complete/econ_significant.csv")
econ_significant <- subset(econ_significant, select = -1)

Completed_data_demorgraphic_ <- read_csv("LM Data and Analysis/Categories Complete/Completed data(demorgraphic).csv")
Completed_data_demorgraphic_ <- subset(Completed_data_demorgraphic_, select = -1)

all_categories_complete <- cbind(matthew_sig_var_complete, healthcare_sigvars_completed, covid_imputed_data_variables, 
                                 enviroment_imputed_data_variables, econ_significant,Completed_data_demorgraphic_)

View(all_categories_complete)

all_categories_complete$income_support <- as.factor(all_categories_complete$income_support)

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

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(all_categories_complete)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, all_categories_complete)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, all_categories_complete)
vif(final_model)

step_all_categories <- step(final_model)

# Check Adjusted R-squared 
summary(step_all_categories)

# We now plot our model assumptions:

par(mfrow = c(2, 2))
plot(step_all_categories)

# Can plot residuals against variables in model to look for patterns e.g. quadratic 

# Or, interactions between variables so may need to include interaction term in model 






