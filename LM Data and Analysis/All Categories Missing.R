# Combining the Significant Variables with Missing Data Together

library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

library(datasets)
library(car)

# VIF Code for Later

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



# Matthew's Data

matthew_sig_var_w_missing <- read_csv("LM Data and Analysis/Categories with Missing/matthew_sig_var_w_missing.csv")
matthew_sig_var_w_missing <- clean_names(matthew_sig_var_w_missing)
matthew_sig_var_w_missing <- subset(matthew_sig_var_w_missing, select = -1)

# Emma's Data

healthcare_sigvars_missing <- read_csv("LM Data and Analysis/Categories with Missing/healthcare_sigvars_missing.csv")
healthcare_sigvars_missing <- clean_names(healthcare_sigvars_missing)
healthcare_sigvars_missing <- subset(healthcare_sigvars_missing, select = -1)

# Andy's Demographic Data - update this with the new data...

Not_Completed_data_demorgraphic_ <- read_csv("LM Data and Analysis/Categories with Missing/Not Completed data(demorgraphic).csv")
Not_Completed_data_demorgraphic_ <- clean_names(Not_Completed_data_demorgraphic_)
Not_Completed_data_demorgraphic_ <- subset(Not_Completed_data_demorgraphic_, select = -1)

# Eman's Economic Data

econ_significant_pre_random_forest <- read_csv("LM Data and Analysis/Categories with Missing/econ_significant_pre_random_forest.csv")
econ_significant_pre_random_forest <- clean_names(econ_significant_pre_random_forest)
econ_significant_pre_random_forest <- subset(econ_significant_pre_random_forest, select = -1)

# Bee Covid Data

covid_clean_data_variables <- read_csv("LM Data and Analysis/Categories with Missing/covid_clean_data_variables.csv")
covid_clean_data_variables <- clean_names(covid_clean_data_variables)
covid_clean_data_variables <- subset(covid_clean_data_variables, select = -1)

# Bee Environment Data

rf_enviroment_clean_data_variables <- read_csv("LM Data and Analysis/Categories with Missing/enviroment_clean_data_variables.csv")
rf_enviroment_clean_data_variables <- clean_names(rf_enviroment_clean_data_variables)
rf_enviroment_clean_data_variables <- subset(rf_enviroment_clean_data_variables, select = -1)


# We now want to combine these dataframes into one big dataframe.

all_cat_with_missing <- cbind(matthew_sig_var_w_missing, econ_significant_pre_random_forest, healthcare_sigvars_missing, Not_Completed_data_demorgraphic_, covid_clean_data_variables, rf_enviroment_clean_data_variables)

write.csv(all_cat_with_missing,"all_cat_with_missing.csv", row.names = TRUE)


# We will now inspect the missing data in the combined significant variables.
vis_miss(all_cat_with_missing, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# We see that 18.1% of our data missing...


# We will now impute these data using random forest imputation method

set.seed(100)
all_cat_rf <- missForest(as.matrix(all_cat_with_missing))
all_cat_imputed <- as.data.frame.matrix(all_cat_rf$ximp)


# We see that we have no missing data now.
vis_miss(all_cat_imputed, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))



# We will now do model selection, and we begin by using VIF

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("healthy_diet_cost_percent_cannot_afford","cost_of_calorie_sufficient_diet_2017_usd_per_day","healthy_diet_cost_percent_of_1_20_poverty_line","life_satisfaction_in_cantril_ladder_world_happiness_report_2019","age_standardised_diabetes_prevalence_male","cardiovascular_diseases_ihme_2017","meningitis_ihme_2017","prevalence_of_obesity_female_who_2019","kidney_disease_ihme_2017","diabetes_blood_and_endocrine_disease_ihme_2017","income_classification_world_bank_2017","gdp_growth_from_previous_year_2020_q2","gdp","national_poverty_lines_jolliffe_and_prydz_2016","percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016","multidimensional_poverty_headcount_ratio_alkire_and_robles_2016","all_causes_disability_adjusted_life_years_who_2015","beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd","long_term_care_beds_per_1_000_population_oecd","publicly_owned_hospitals_per_million_population_oecd","surgical_specialists_per_1_000_population_oecd","infant_mortality_rate","income_support","containment_index","yll_rates_from_all_air_pollution_per_100_000","death_rates_from_all_air_pollution_per_100_000")
after_drop <- gvif_drop(resp, expl, all_cat_imputed)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, all_cat_imputed)
vif(final_model)

step_all_cat_imputed <- step(final_model)
summary(step_all_cat_imputed)



par(mfrow = c(2, 2))
plot(step_all_cat_imputed)



