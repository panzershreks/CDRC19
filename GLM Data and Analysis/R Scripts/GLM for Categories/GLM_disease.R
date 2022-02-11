# GLM Matthew Categories

library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

combined_all_missing <- read_csv("GLM Data and Analysis/Combined CSV/combined_all_missing.csv")
combined_all_missing <- clean_names(combined_all_missing)
combined_all_missing <- subset(combined_all_missing, select = -1)

# now subset disease variables and response
# response = column 1
# disease = 19 -> 46
# food/water = 2 -> 18 + world stats = 47

disease_missing <- subset(combined_all_missing, select = c(1,19:46))

set.seed(100)
disease_rf <- missForest(as.matrix(disease_missing))
disease_imputed <- as.data.frame.matrix(disease_rf$ximp)

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
expl <- colnames(disease_imputed)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, disease_imputed)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, disease_imputed, family = Gamma(link = "log"))
vif(final_model)

after_drop

step_final_model <- step(final_model)

summary(step_final_model)

# We now want to take the variables output from the step function and put them into a dataframe.
# First we do this for the imputed data

disease_imputed_sig_var <- subset(disease_imputed, select = c(2,8,18,21,24,29))

write.csv(disease_imputed_sig_var,"GLM Data and Analysis//Significant Variables//Categories Complete//disease_imputed_sig_var.csv", row.names = TRUE)


# Now we do this for the data brefore random forests


disease_missing_sig_var <- subset(disease_missing, select = c(2,8,18,21,24,29))

write.csv(disease_missing_sig_var,"GLM Data and Analysis//Significant Variables//Categories with Missing//disease_missing_sig_var.csv", row.names = TRUE)









