# VIF Testing Function

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
library('missForest')
library(readr)

# Function written by Eman:

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

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -c(1,2))

set.seed(100)
food_rf <- missForest(as.matrix(clean_food_water))

food_data <- food_rf$ximp
food_data <- as.data.frame.matrix(food_data)


resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("cost_of_calorie_sufficient_diet_2017_usd_per_day",
          "cost_of_nutrient_adequate_diet_2017_usd_per_day",
          "cost_of_healthy_diet_2017_usd_per_day",
          "calorie_sufficient_diet_cost_percent_of_1_20_poverty_line",
          "nutrient_adequate_diet_cost_percent_of_1_20_poverty_line",
          "healthy_diet_cost_percent_of_1_20_poverty_line",
          "calorie_sufficient_diet_cost_percent_of_average_food_expenditure",
          "nutrient_adequate_diet_cost_percent_of_average_food_expenditure",
          "healthy_diet_cost_percent_of_average_food_expenditure",
          "calorie_sufficient_diet_cost_percent_cannot_afford",
          "nutrient_adequate_diet_cost_percent_cannot_afford",
          "healthy_diet_cost_percent_cannot_afford",
          "calorie_sufficient_diet_cost_number_cannot_afford",
          "nutrient_adequate_diet_cost_number_cannot_afford",
          "healthy_diet_cost_number_cannot_afford",
          "population_with_access_to_improved_sanitation_y",
          "population_without_access_to_improved_sanitation_y")

after_drop <- gvif_drop(resp, expl, food_data)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, food_data)
vif(final_model)











