library(glm2)
library(MuMIn)

#' Perform stepwise backwards selection for GLMs
#' Uses glm2, which is identical to glm, but computationally different in order
#' to avoid the errors associating from computation in glm
#' @param resp str of response variable
#' @param expl list of str of explanatory variables
#' @param data
#' @param criterion takes args AIC, BIC, AICc
#' @param family family for GLM
#' @return model after backwards selection
step2.glm <- function(resp, expl, data, criterion, family, maxit=25) {
  formula_og <- lm_formula_paster(resp, expl)
  mod_og <- glm2(formula=formula_og, data=data, family=family, maxit=maxit)
  aic_og <- aic_helper(mod_og, criterion)
  aic_l <- c()
  if (length(expl) > 1) {
    for (i in 1:length(expl)) {
      expl_tmp <- expl[-i]
      formula_tmp <- lm_formula_paster(resp, expl_tmp)
      mod_tmp <- glm2(formula=formula_tmp, data=data, family=family, maxit=maxit)
      aic_tmp <- aic_helper(mod_tmp, criterion)
      aic_l <- c(aic_l, aic_tmp)
    }
    if (min(aic_l) < aic_og) {
      rm_index <- which.min(aic_l)
      print(paste0("REMOVE: ", expl[rm_index]))
      expl <- expl[-rm_index]
      step2.glm(resp, expl, data, criterion, family, maxit)
    } else {
      return(mod_og)
    }
  } else {
    return(mod_og)
  }
}

#' Relevant AIC, BIC or AICc for model
#' @param model model object
#' @param criterion str, either: "AIC", "BIC", or "AICc"
#' @return relevant value
aic_helper <- function(model, criterion){
  if (criterion == 'AIC') {
    return(AIC(model))
  } 
  if (criterion == 'BIC') {
    return(BIC(model))
  } 
  if (criterion == "AICc") {
    return(AICc(model))
  }
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
