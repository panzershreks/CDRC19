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

# Load in imputed data
healthcare_sigvars_completed <- read_csv("LM Data and Analysis/Categories Complete/healthcare_sigvars_completed.csv")
healthcare_sigvars_completed <- subset(healthcare_sigvars_completed, select = -1)
matthew_sig_var_complete <- read_csv("LM Data and Analysis/Categories Complete/matthew_sig_var_complete.csv")
matthew_sig_var_complete <- subset(matthew_sig_var_complete, select = -1)
enviroment_imputed_data_variables <- read_csv("LM Data and Analysis/Categories Complete/enviroment_imputed_data_variables.csv")
enviroment_imputed_data_variables <- subset(enviroment_imputed_data_variables, select = -1)
covid_imputed_data_variables <- read_csv("LM Data and Analysis/Categories Complete/covid_imputed_data_variables.csv")
covid_imputed_data_variables <- subset(covid_imputed_data_variables, select = -1)
econ_significant <- read_csv("LM Data and Analysis/Categories Complete/econ_significant.csv")
econ_significant <- subset(econ_significant, select = -1)
Completed_data_demorgraphic_ <- read_csv("LM Data and Analysis/Categories Complete/Completed data(demorgraphic).csv")
Completed_data_demorgraphic_ <- subset(Completed_data_demorgraphic_, select = -1)
all_categories_complete <- cbind(matthew_sig_var_complete, healthcare_sigvars_completed, covid_imputed_data_variables, 
                                 enviroment_imputed_data_variables, econ_significant,Completed_data_demorgraphic_)
all_categories_complete$income_support <- as.factor(all_categories_complete$income_support)

# Fomula selected from "All categories completed_LM.R"
lm_step_formula <- total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
  healthy_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_of_1_20_poverty_line + 
  life_satisfaction_in_cantril_ladder_world_happiness_report_2019 + 
  prevalence_of_obesity_female_who_2019 + diabetes_blood_and_endocrine_disease_ihme_2017 + 
  beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd + 
  publicly_owned_hospitals_per_million_population_oecd + 
  surgical_specialists_per_1_000_population_oecd + income_support + 
  gdp_growth_per_capita_from_previous_year_2020_q2 + percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 + 
  poverty_rate_50_percent_of_median_lis_key_figures_2018

# GLMs
glm_mod_gamma_log <- glm(formula = lm_step_formula,
                         family = Gamma(link = "log"),
                         data = all_categories_complete)
par(mfrow = c(2, 2))
plot(glm_mod_gamma_log, main="Gamma, Link=log")

glm_mod_quasipois <- glm(formula = lm_step_formula,
                         family = quasipoisson,
                         data = all_categories_complete)
par(mfrow = c(2, 2))
plot(glm_mod_quasipois, main="quasipois")

glm_mod_quasipois_inv <- glm(formula = lm_step_formula,
                             family = quasipoisson(link = "inverse"),
                             data = all_categories_complete)
par(mfrow = c(2, 2))
plot(glm_mod_quasipois_inv, main="quasipois, Link=inverse")

glm(formula = lm_step_formula,
    family = Gamma,
    data = all_categories_complete)

step(glm_mod_gamma_log)

foo <- glm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
      healthy_diet_cost_percent_cannot_afford + life_satisfaction_in_cantril_ladder_world_happiness_report_2019 + 
      prevalence_of_obesity_female_who_2019 + publicly_owned_hospitals_per_million_population_oecd + 
      surgical_specialists_per_1_000_population_oecd + income_support + 
      gdp_growth_per_capita_from_previous_year_2020_q2 + poverty_rate_50_percent_of_median_lis_key_figures_2018, 
    family = Gamma(link = "log"), data = all_categories_complete)
par(mfrow = c(2, 2))
plot(foo, main="foo")

par(mfrow = c(1, 1))
plot(x=fitted(foo), y=all_categories_complete$total_confirmed_deaths_due_to_covid_19_per_million_people,xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')


rf_mod <- randomForest(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
               healthy_diet_cost_percent_cannot_afford + life_satisfaction_in_cantril_ladder_world_happiness_report_2019 + 
               prevalence_of_obesity_female_who_2019 + publicly_owned_hospitals_per_million_population_oecd + 
               surgical_specialists_per_1_000_population_oecd + income_support + 
               gdp_growth_per_capita_from_previous_year_2020_q2 + poverty_rate_50_percent_of_median_lis_key_figures_2018,
             data = all_categories_complete)




