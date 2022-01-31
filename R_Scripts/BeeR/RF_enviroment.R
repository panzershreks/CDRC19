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

clean_enviroment <- read.csv("~/Documents/University/YEAR 4/ALL YEAR/MATHEMATICAL PROJECT/ANALYSIS/CDRC19/Combined DataFrame Work/CSV Files/Clean/clean_enviroment.csv")
clean_enviroment <- clean_names(clean_enviroment)
clean_enviroment <- subset(clean_enviroment, select = -c(1,2))
clean_enviroment <- clean_enviroment[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                              145, 151, 156, 171,173,177,178, 186,193), ]

set.seed(100)
enviroment_rf <- missForest(as.matrix(clean_enviroment))
enviroment_data <- enviroment_rf$ximp
enviroment_data <- as.data.frame.matrix(enviroment_data)

write.csv(enviroment_data, file = "rf_enviroment_data.csv", row.names = TRUE)
resp_var_env <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl_var_env <- c("indoor_10_to_14_years", "indoor_15_to_19_years", "
  indoor_20_to_24_years", "
  indoor_25_to_29_years", "
  indoor_30_to_34_years", "
  indoor_35_to_39_years", "
  indoor_40_to_44_years", "
  indoor_45_to_49_years", "
  indoor_5_to_9_years", "
  indoor_50_to_54_years", "
  indoor_55_to_59_years", "
  indoor_60_to_64_years", "
  indoor_65_to_69_years", "
  indoor_70_to_74_years", "
  indoor_75_to_79_years", "
  indoor_80_years", "
  indoor_under_5s", "
  outdoor_10_to_14_years", "
  outdoor_15_to_19_years", "
  outdoor_20_to_24_years", "
  outdoor_25_to_29_years", "
  outdoor_30_to_34_years", "
  outdoor_35_to_39_years", "
  outdoor_40_to_44_years", "
  outdoor_45_to_49_years", "
  outdoor_5_to_9_years", "
  outdoor_50_to_54_years", "
  outdoor_55_to_59_years", "
  outdoor_60_to_64_years", "
  outdoor_65_to_69_years", "
  outdoor_70_to_74_years", "
  outdoor_75_to_79_years", "
  outdoor_80_years", "
  outdoor_under_5s", "
  excess_mortality_from_air_pollution_all_sources", "
  excess_mortality_from_fossil_fuels", "
  excess_mortality_from_all_anthropogenic_pollution", "
  total_years_life_lost_from_air_pollution_all_sources", "
  total_years_life_lost_from_fossil_fuels", "
  total_years_life_lost_from_all_anthropogenic_pollution", "
  death_rates_from_all_air_pollution_per_100_000", "
  death_rates_from_air_pollution_from_fossil_fuels_per_100_000", "
  death_rates_from_all_anthropogenic_air_pollution_per_100_000", "
  yll_rates_from_all_air_pollution_per_100_000", "
  yll_rates_from_air_pollution_from_fossil_fuels_per_100_000", "
  yll_rates_from_anthropogenic_air_pollution_per_100_000", "
  deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths", "
  deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths",
              "deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths")

after_drop_env <- gvif_drop(resp_var_env, expl_var_env, enviroment_data)
final_formula_env <- lm_formula_paster(resp_var_env, after_drop_env)
final_model_env <- lm(final_formula_env, enviroment_data)
vif(final_model_env)
stargazer(vif(final_model_env), type='latex', summary=FALSE)
step(final_model_env)
summary(final_model_env)
plot(final_model_env)

#lm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#yll_rates_from_all_air_pollution_per_100_000 + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths + 
#  deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = enviroment_data)
