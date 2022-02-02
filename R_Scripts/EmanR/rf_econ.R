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

# Import data and basic pre-processing
# convert entities to factors and all other columns to numeric, coerce to NAs
econ_data_full <- read_csv("./../../Combined DataFrame Work/CSV Files/Clean/clean_economic.csv")
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

# Percentage of missing values
missing_vars_tbl <- miss_var_summary(clean_econ, sort_miss = TRUE)
missing_vars_tbl

set.seed(100)
econ_rf <- missForest(data.frame(clean_econ))

# We save our imputed dataset as a dataframe
econ_df_rf <- as.data.frame.matrix(econ_rf$ximp)

miss_econ_df_rf <- miss_var_summary(econ_df_rf, sort_miss = TRUE)
miss_econ_df_rf

# import automate vif functions
source("automate_vif.R") 
# e.g. usage
# data(iris)
# resp <- "Sepal.Length"
# expl <- c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
# after_drop <- gvif_drop(resp, expl, iris)
# final_formula <- lm_formula_paster(resp, after_drop)
# final_model <- lm(final_formula, iris)
# vif(final_model)

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("mean", "pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
          "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
          "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
          "q3avgincome", "q4avgincome", "q5avgincome", "flag",
          "income_classification_world_bank_2017", "gini_index",
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
             "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y")

expl_dr <- expl[!expl %in% to_drop]

after_drop <- gvif_drop(resp, expl_dr, econ_df_rf)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- lm(drop_vif_formula, econ_df_rf)

stepAIC <- step(model_drop_vif, direction="backward")
stepAIC_names <- names(stepAIC$coefficients)[-1]
stepBIC <- step(model_drop_vif, direction="backward", k=log(nrow(econ_df_rf)))
stepBIC_names <- names(stepBIC$coefficients)[-1]
stepAIC_names
stepBIC_names

summary(stepAIC)
summary(stepBIC)
anova(stepAIC, stepBIC)

# AIC significant variables 
# income_classification_world_bank_2017
# gdp_growth_from_previous_year_2020_q2
# gdp
# national_poverty_lines_jolliffe_and_prydz_2016
# percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016

# BIC significant variables
# gdp_growth_from_previous_year_2020_q2
# gdp
# national_poverty_lines_jolliffe_and_prydz_2016
# multidimensional_poverty_headcount_ratio_alkire_and_robles_2016

sig_econ_df <- subset(econ_df_rf, select=c(
  income_classification_world_bank_2017,
  gdp_growth_from_previous_year_2020_q2,
  gdp,
  national_poverty_lines_jolliffe_and_prydz_2016,
  percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016,
  multidimensional_poverty_headcount_ratio_alkire_and_robles_2016
))

sig_econ_df_pre_rd <- subset(clean_econ, select=c(
  income_classification_world_bank_2017,
  gdp_growth_from_previous_year_2020_q2,
  gdp,
  national_poverty_lines_jolliffe_and_prydz_2016,
  percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016,
  multidimensional_poverty_headcount_ratio_alkire_and_robles_2016
))

write.csv(sig_econ_df, file="econ_significant.csv")
write.csv(sig_econ_df_pre_rd, file="econ_significant_pre_random_forest.csv")

