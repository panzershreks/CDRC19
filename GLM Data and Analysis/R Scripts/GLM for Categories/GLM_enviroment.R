library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(tidyverse)
library(readr)
library(dplyr)
library(naniar)
library(car)
library(visdat)
library(mice)
library(UpSetR)
library(janitor)
library(corrplot)
library(gt)
library(missForest)
library(stargazer)

glm_clean_enviroment <- read.csv("~/Documents/University/YEAR 4/ALL YEAR/MATHEMATICAL PROJECT/ANALYSIS/CDRC19/Combined DataFrame Work/CSV Files/Clean/clean_enviroment.csv")
glm_clean_enviroment <- clean_names(glm_clean_enviroment)
glm_clean_enviroment <- subset(glm_clean_enviroment, select = -c(1,2))
glm_clean_enviroment <- glm_clean_enviroment[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                        145, 151, 156, 171,173,177,178, 186,193), ]
glm_clean_enviroment$indoor_5_to_49 <- glm_clean_enviroment$indoor_5_to_9_years + glm_clean_enviroment$indoor_10_to_14_years + glm_clean_enviroment$indoor_15_to_19_years +glm_clean_enviroment$indoor_20_to_24_years + glm_clean_enviroment$indoor_25_to_29_years + glm_clean_enviroment$indoor_30_to_34_years + glm_clean_enviroment$indoor_35_to_39_years + glm_clean_enviroment$indoor_40_to_44_years + glm_clean_enviroment$indoor_45_to_49_years
glm_clean_enviroment$indoor_50_to_79 <- glm_clean_enviroment$indoor_50_to_54_years + glm_clean_enviroment$indoor_55_to_59_years +glm_clean_enviroment$indoor_60_to_64_years + glm_clean_enviroment$indoor_65_to_69_years + glm_clean_enviroment$indoor_70_to_74_years + glm_clean_enviroment$indoor_75_to_79_years 
glm_clean_enviroment$outdoor_5_to_49 <- glm_clean_enviroment$outdoor_5_to_9_years + glm_clean_enviroment$outdoor_10_to_14_years + glm_clean_enviroment$outdoor_15_to_19_years +glm_clean_enviroment$outdoor_20_to_24_years + glm_clean_enviroment$outdoor_25_to_29_years + glm_clean_enviroment$outdoor_30_to_34_years + glm_clean_enviroment$outdoor_35_to_39_years + glm_clean_enviroment$outdoor_40_to_44_years + glm_clean_enviroment$outdoor_45_to_49_years
glm_clean_enviroment$outdoor_50_to_79 <- glm_clean_enviroment$outdoor_50_to_54_years + glm_clean_enviroment$outdoor_55_to_59_years +glm_clean_enviroment$outdoor_60_to_64_years + glm_clean_enviroment$outdoor_65_to_69_years + glm_clean_enviroment$outdoor_70_to_74_years + glm_clean_enviroment$outdoor_75_to_79_years 
drop_4 <- c("indoor_10_to_14_years", "indoor_15_to_19_years", "indoor_20_to_24_years", 
            "indoor_25_to_29_years", "indoor_30_to_34_years", "indoor_35_to_39_years", 
            "indoor_40_to_44_years", "indoor_45_to_49_years", "indoor_5_to_9_years", 
            "indoor_50_to_54_years", "indoor_55_to_59_years", "indoor_60_to_64_years", 
            "indoor_65_to_69_years", "indoor_70_to_74_years", "indoor_75_to_79_years", 
            "outdoor_10_to_14_years", "outdoor_15_to_19_years", "outdoor_20_to_24_years", 
            "outdoor_25_to_29_years", "outdoor_30_to_34_years", "outdoor_35_to_39_years", 
            "outdoor_40_to_44_years", "outdoor_45_to_49_years", "outdoor_5_to_9_years", 
            "outdoor_50_to_54_years", "outdoor_55_to_59_years", "outdoor_60_to_64_years", 
            "outdoor_65_to_69_years", "outdoor_70_to_74_years", "outdoor_75_to_79_years")
glm_clean_enviroment <- glm_clean_enviroment[,!(names(glm_clean_enviroment) %in% drop_4)]
#write.csv(glm_clean_enviroment, file = "glm_clean_enviroment.csv", row.names = TRUE)
#enviroment_missing_vis <- vis_miss(glm_clean_enviroment, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
#enviroment_missing_vis
#summary(glm_clean_enviroment)
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
set.seed(100)
glm_enviroment_rf <- missForest(as.matrix(glm_clean_enviroment))
glm_enviroment_data <- glm_enviroment_rf$ximp
glm_enviroment_data <- as.data.frame.matrix(glm_enviroment_data)
drop_5 <- c("excess_mortality_from_fossil_fuels",	
            "excess_mortality_from_all_anthropogenic_pollution",
            "total_years_life_lost_from_air_pollution_all_sources",
            "total_years_life_lost_from_fossil_fuels",
            "total_years_life_lost_from_all_anthropogenic_pollution",
            "death_rates_from_air_pollution_from_fossil_fuels_per_100_000",
            "death_rates_from_all_anthropogenic_air_pollution_per_100_000",
            "yll_rates_from_air_pollution_from_fossil_fuels_per_100_000",
            "yll_rates_from_anthropogenic_air_pollution_per_100_000",
            "deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths",
            "deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths",
            "deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths")
glm_enviroment_data <- glm_enviroment_data[,!(names(glm_enviroment_data) %in% drop_5)]
glm_enviroment_data
#write.csv(glm_enviroment_data, file = "enviroment_all_missing.csv", row.names = TRUE)
#Mcor <- glm_enviroment_data
#enviroment_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
#  ggtitle("Correlation Matrix")
#enviroment_cor_matrix
# write.csv(glm_enviroment_data, file = "rf_glm_enviroment_data.csv", row.names = TRUE)
resp_var_env <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl_var_env <- c("indoor_5_to_49", "indoor_50_to_79", "
  indoor_80_years", "
  indoor_under_5s", "
  outdoor_5_to_49", "
  outdoor_50_to_79", "
  outdoor_80_years", "
  outdoor_under_5s", "
  excess_mortality_from_air_pollution_all_sources", "
  death_rates_from_all_air_pollution_per_100_000", 
                  "yll_rates_from_all_air_pollution_per_100_000")

after_drop_env <- gvif_drop(resp_var_env, expl_var_env, glm_enviroment_data)
glm_final_formula_env <- lm_formula_paster(resp_var_env, after_drop_env)
glm_final_model_env <- glm(glm_final_formula_env, glm_enviroment_data, family = Gamma(link = "log"))
vif(glm_final_model_env)
# stargazer(vif(final_model_env), type='latex', summary=FALSE)
step_glm_final_model_env <- step(glm_final_model_env)
summary(step_glm_final_model_env)
par(mfrow = c(2, 2))
plot(step_glm_final_model_env)

#glm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#death_rates_from_all_air_pollution_per_100_000 + yll_rates_from_all_air_pollution_per_100_000, 
#family = Gamma(link = "log"), data = glm_enviroment_data)

# only include variables with significance level 0.05 of more... these include:
# "yll_rates_from_all_air_pollution_per_100_000", "death_rates_from_all_air_pollution_per_100_000"
#glm_enviroment_imputed_data_variables <- subset(glm_enviroment_data, select = c("yll_rates_from_all_air_pollution_per_100_000", "death_rates_from_all_air_pollution_per_100_000"))
#write.csv(glm_enviroment_imputed_data_variables, file = "glm_enviroment_imputed_data_variables.csv", row.names = TRUE)

# now another dataset with the significant variables from the data before it was imputed
#glm_enviroment_clean_data_variables <- subset(glm_clean_enviroment, select = c("yll_rates_from_all_air_pollution_per_100_000", "death_rates_from_all_air_pollution_per_100_000"))
#write.csv(glm_enviroment_clean_data_variables, file = "glm_enviroment_clean_data_variables.csv", row.names = TRUE)

