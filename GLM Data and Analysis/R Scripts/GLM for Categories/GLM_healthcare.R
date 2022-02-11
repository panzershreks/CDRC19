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

healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")

# Remove first column 
healthcare_all_missing_clean <- subset(healthcare_all_mis, select = -c(1,2))

healthcare_all_missing_clean <- clean_names(healthcare_all_missing_clean)

set.seed(100)

healthcare_all_rf <- missForest(as.matrix(healthcare_all_missing_clean))
healthcare_imputed_rf <- as.data.frame.matrix(healthcare_all_rf$ximp)

# GLM Model VIF Function :

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
expl <- colnames(healthcare_imputed_rf)
expl <- expl[-1]

after_drop <- gvif_drop(resp, expl, healthcare_imputed_rf)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, healthcare_imputed_rf, family = Gamma(link = "log"))
vif(final_model)

after_drop

step_final_model <- step(final_model)

summary(step_final_model)

plot(step_final_model)

# Take variables from step function and create new dataframe (these have missing values )
healthcare_sigvars_missing_GLM <- subset(healthcare_all_missing_clean, select = c("all_causes_disability_adjusted_life_years_who_2015", 
                                                                                "share_of_population_covered_by_health_insurance_ilo_2014", 
                                                                                "beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd", 
                                                                                "beds_in_publicly_owned_hospitals_per_1_000_population_oecd", 
                                                                                "nurses_per_1_000_population_oecd", 
                                                                                "psychiatric_care_beds_per_1_000_population_oecd", 
                                                                                "publicly_owned_hospitals_per_million_population_oecd", 
                                                                                "surgical_specialists_per_1_000_population_oecd", 
                                                                                "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure", 
                                                                                "total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors")) 

write.csv(healthcare_sigvars_missing_GLM, file = "healthcare_sigvars_missing_GLM.csv", row.names = TRUE)

# Subset after imputation now 

healthcare_sigvars_imputed_GLM <- subset(healthcare_imputed_rf, select = c("all_causes_disability_adjusted_life_years_who_2015", 
                                                                    "share_of_population_covered_by_health_insurance_ilo_2014", 
                                                                    "beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd", 
                                                                    "beds_in_publicly_owned_hospitals_per_1_000_population_oecd", 
                                                                    "nurses_per_1_000_population_oecd", 
                                                                    "psychiatric_care_beds_per_1_000_population_oecd", 
                                                                    "publicly_owned_hospitals_per_million_population_oecd", 
                                                                    "surgical_specialists_per_1_000_population_oecd", 
                                                                    "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure", 
                                                                    "total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors"))

write.csv(healthcare_sigvars_imputed_GLM, file = "healthcare_sigvars_imputed_GLM.csv", row.names = TRUE)
