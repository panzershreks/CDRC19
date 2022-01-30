library(datasets)
library(car)

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=10) {
    gvif_max <- vif_max ^ 0.5
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- lm(lm_formula, data)
    vif_mod <- vif(model)
    while (max(vif_mod[,3]) > gvif_max) {
        expl_var <- expl_var[-(which.max(vif_mod[,3]))]
        lm_formula <- lm_formula_paster(resp_var, expl_var)
        model <- lm(lm_formula, data)
        vif_mod <- vif(model)
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
    form <- paste0(form, tail(expl, 1))
    return (form)
}

data(iris)

resp <- "Sepal.Length"
expl <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")

after_drop <- gvif_drop(resp, expl, iris)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, iris)
vif(final_model)


# If we did this manually:
model1 <- lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species, iris)
vif(model1) 
# remove Petal.Length
model2 <- lm(Sepal.Length~Sepal.Width+Petal.Width+Species, iris)
vif(model2)
# remove Petal.Width
model3 <- lm(Sepal.Length~Sepal.Width+Species, iris)
vif(model3) # final model
















