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

Completed_data_demorgraphic_ <- read_csv("Final Model Data and Analysis/Categories Complete/Completed data(demorgraphic).csv")
Completed_data_demorgraphic_ <- subset(Completed_data_demorgraphic_, select = -1)

all_categories_complete <- cbind(matthew_sig_var_complete, healthcare_sigvars_completed, rf_covid_imputed_data_variables, 
                                 rf_enviroment_imputed_data_variables, econ_significant,Completed_data_demorgraphic_)
  
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
expl <- c("age_standardised_diabetes_prevalence_male",
          "cardiovascular_diseases_ihme_2017",
          "meningitis_ihme_2017",
          "prevalence_of_obesity_female_who_2019",
          "healthy_diet_cost_percent_cannot_afford",
          "share_of_people_who_disagree_vaccines_are_important_for_children_to_have",
          "share_of_people_who_disagree_vaccines_are_safe",
          "share_of_people_who_agree_vaccines_are_effective",
          "general_hospitals_per_million_population_oecd",
          "not_for_profit_privately_owned_hospitals_per_million_population_oecd",
          "psychiatric_care_beds_per_1_000_population_oecd",
          "publicly_owned_hospitals_per_million_population_oecd",
          "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure",
          "case_fatality_rate_of_covid_19_short_term",
          "biweekly_deaths_per_million_people",
          "weekly_cases_per_million_people",
          "weekly_case_growth",
          "doubling_days_of_total_confirmed_deaths_3_day_period",
          "days_since_10_daily_new_confirmed_deaths_recorded",
          "case_fatality_rate_of_covid_19_only_observations_with_100_cases",
          "days_since_30_daily_new_confirmed_cases_recorded",
        # "total_confirmed_deaths_due_to_covid_19",
          "daily_new_confirmed_cases_of_covid_19_per_million_people",
          "daily_new_confirmed_deaths_due_to_covid_19_per_million_people",
          "total_confirmed_cases_of_covid_19_per_million_people",
          "population_with_access_to_improved_sanitation_x",
          "debt_relief",
          "stringency_index",
          "yll_rates_from_anthropogenic_air_pollution_per_100_000",
          "deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths", 
          "deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths",
          "income_classification_world_bank_2017",
          "gdp_growth_from_previous_year_2020_q2",
          "gdp",
          "national_poverty_lines_jolliffe_and_prydz_2016",
          "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
          "multidimensional_poverty_headcount_ratio_alkire_and_robles_2016",
          "Infant.mortality.rate")

after_drop <- gvif_drop(resp, expl, all_categories_complete)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, all_categories_complete)
vif(final_model)

step_all_categories <- step(final_model)
summary(step_all_categories)

# We now plot our model assumptions:

par(mfrow = c(2, 2))
plot(step_all_categories)


colnames(all_categories_complete)


