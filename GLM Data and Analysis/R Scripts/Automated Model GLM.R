library(datasets)
library(car)
library(readr)
library("janitor")
library('missForest')
library(glm2)
library(MuMIn)

source("R_Scripts/EmanR/step2.R")

# We read in the imputed data

combined_imputed <- read_csv("GLM Data and Analysis/Combined CSV/combined_imputed.csv")
combined_imputed <- clean_names(combined_imputed)
combined_imputed <- subset(combined_imputed, select = -1)

combined_imputed$income_support <- as.factor(combined_imputed$income_support)
combined_imputed$debt_relief <- as.factor(combined_imputed$debt_relief)
combined_imputed$income_classification_world_bank_2017 <- as.factor(combined_imputed$income_classification_world_bank_2017)

# We now run the GLM Model Function:

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=5) {
  gvif_max <- vif_max ^ 0.5
  lm_formula <- lm_formula_paster(resp_var, expl_var)
  model <- glm(lm_formula, data, family = Gamma(link ="log"), maxit=100)
  vif_mod <- vif(model)
  try(gvif <- vif_mod, silent = TRUE)
  try(gvif <- vif_mod[,3], silent = TRUE)
  if (is.null(dim(vif_mod))) {
    gvif_max <- vif_max
  }
  while (max(gvif) > gvif_max) {
    expl_var <- expl_var[-(which.max(gvif))]
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- glm(lm_formula, data, family = Gamma(link ="log"), maxit=100)
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
expl <- colnames(combined_imputed)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, combined_imputed, vif_max=6)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, combined_imputed, family = Gamma(link = "log"))
vif(final_model)

after_drop

step_AIC_mod <- step2.glm(resp, after_drop, combined_imputed, "AIC", Gamma(link="log"), maxit=25)
step_BIC_mod <- step2.glm(resp, after_drop, combined_imputed, "BIC", Gamma(link="log"), maxit=25)
step_AICc_mod <- step2.glm(resp, after_drop, combined_imputed, "AICc", Gamma(link="log"), maxit=25)

summary(step_AIC_mod)
summary(step_BIC_mod)
summary(step_AICc_mod)

par(mfrow = c(2,2))
plot(step_AIC_mod, main="AIC_model")
plot(step_BIC_mod, main="BIC_model")
plot(step_AICc_mod, main="AICc_model")






