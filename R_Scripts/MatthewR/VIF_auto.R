library(datasets)
library(car)

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
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

data(iris)

resp <- "Sepal.Length"
expl <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")

after_drop <- gvif_drop(resp, expl, iris)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, iris)
vif(final_model)

# If we did this manually (when categorical variable is present)
model1 <- lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species, iris)
vif(model1) 
# remove Petal.Length
model2 <- lm(Sepal.Length~Sepal.Width+Petal.Width+Species, iris)
vif(model2)
# remove Petal.Width
model3 <- lm(Sepal.Length~Sepal.Width+Species, iris)
vif(model3) # final model


resp2 <- "Sepal.Length"
expl2 <- c("Sepal.Width", "Petal.Length", "Petal.Width")

after_drop2 <- gvif_drop(resp2, expl2, iris)
final_formula2 <- lm_formula_paster(resp2, after_drop2)
final_model2 <- lm(final_formula2, iris)
vif(final_model2)

# (when there are no categorical variables)
modelA <- lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, iris)
vif(modelA)
# remove Petal.Length
modelB <- lm(Sepal.Length~Sepal.Width+Petal.Width, iris)
vif(modelB) # final model








