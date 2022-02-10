library("ggplot2")
library(grid)
library(gridExtra)
library(lattice)
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library("mice")
library(UpSetR)
library("janitor")
library(corrplot)
library(gt)
library(tictoc)
library(miceRanger)
library(olsrr)
library(missForest)
library(caret)

# Import data and basic pre-processing
# convert entities to factors and all other columns to numeric, coerce to NAs
econ_data_full <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_economic.csv")
econ_data_full <- clean_names(econ_data_full)
entity_col <- subset(econ_data_full, select = entity)
econ_data_full <- subset(econ_data_full, select = -c(x1, entity))
econ_data_full[] <- lapply(econ_data_full, function(x) as.numeric(as.character(x)))
econ_data_full$entity <- entity_col$entity
econ_data_full$entity <- as.factor(econ_data_full$entity)
econ_data_full <- econ_data_full %>% relocate(entity)

# remove rows where response variable is NA
clean_econ <- econ_data_full %>% drop_na(total_confirmed_deaths_due_to_covid_19_per_million_people)
entity_col <- subset(clean_econ, select = entity)
clean_econ <- subset(clean_econ, select = -c(entity))
clean_econ$income_classification_world_bank_2017 <- as.factor(clean_econ$income_classification_world_bank_2017)
clean_econ$flag <- as.factor(clean_econ$flag)

# Percentage of missing values
missing_vars_tbl <- miss_var_summary(clean_econ, sort_miss = TRUE)
missing_vars_tbl

set.seed(100)
econ_rf <- missForest(data.frame(clean_econ))

# We save our imputed dataset as a dataframe
econ_df_rf <- as.data.frame.matrix(econ_rf$ximp)

econ_df_rf$income_classification_world_bank_2017 <- as.factor(econ_df_rf$income_classification_world_bank_2017)
econ_df_rf$flag <- as.factor(econ_df_rf$flag)

miss_econ_df_rf <- miss_var_summary(econ_df_rf, sort_miss = TRUE)
miss_econ_df_rf

names(econ_df_rf)[names(econ_df_rf) == 'mean'] <- 'mean_monthly_income'
names(clean_econ)[names(clean_econ) == 'mean'] <- 'mean_monthly_income'
clean_econ$gdp_growth_per_capita_from_previous_year_2020_q2 <- with(clean_econ, gdp_growth_from_previous_year_2020_q2 / pop2021)

econ_df_rf$gdp_growth_per_capita_from_previous_year_2020_q2 <- with(econ_df_rf, gdp_growth_from_previous_year_2020_q2 / pop2021)

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("mean_monthly_income", "pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
          "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
          "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
          "q3avgincome", "q4avgincome", "q5avgincome", "flag",
          "income_classification_world_bank_2017", "gini_index",
          "gdp_growth_per_capita_from_previous_year_2020_q2",
          "gdp_growth_from_previous_year_2020_q2", "gdp_per_capita_ppp_2011_wdi_2016",
          "pop2021", "gini_coefficient_world_bank_2016", "gdp_per_capita","population_y", "gdp",
          "cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017",
          "national_poverty_lines_jolliffe_and_prydz_2016",
          "multidimensional_poverty_headcount_ratio_alkire_and_robles_2016", 
          "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016",
          "mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "poverty_rate_50_percent_of_median_lis_key_figures_2018")

to_drop <- c("pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
             "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
             "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
             "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y",
             "gdp_per_capita_ppp_2011_wdi_2016", "gdp", "gdp_growth_from_previous_year_2020_q2",
             "pop2021", "gini_coefficient_world_bank_2016", 
             "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
             "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
             "percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016",
             "percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016")

expl_dr <- expl[!expl %in% to_drop]

# We now run the GLM Model Function:

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=7) {
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

after_drop <- gvif_drop(resp, expl_dr, econ_df_rf, vif_max=7)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, econ_df_rf, family = Gamma(link = "log"), maxit=100)
vif(model_drop_vif)

step_drop_vif <- step(model_drop_vif)

summary(step_drop_vif)

# mean_monthly_income
# gdp_growth_per_capita_from_previous_year_2020_q2
# multidimensional_poverty_headcount_ratio_alkire_and_robles_2016
# poverty_rate_50_percent_of_median_lis_key_figures_2018  

plot(step_drop_vif)











