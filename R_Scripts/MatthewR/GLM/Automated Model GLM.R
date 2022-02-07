library(datasets)
library(car)
library(readr)
library("janitor")
library('missForest')

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=5) {
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

data(iris)

resp <- "Sepal.Length"
expl <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")

after_drop <- gvif_drop(resp, expl, iris)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, iris, family = Gamma(link = "log"))
vif(final_model)



