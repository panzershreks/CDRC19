library(datasets)
library(car)
library(readr)
library("janitor")
library('missForest')
library(glm2)
library(MuMIn)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

# We read in the imputed data

combined_imputed <- read_csv("GLM Data and Analysis/Combined CSV/combined_imputed.csv")
combined_imputed <- clean_names(combined_imputed)
combined_imputed <- subset(combined_imputed, select = -1)

combined_imputed$income_support <- as.factor(combined_imputed$income_support)
combined_imputed$debt_relief <- as.factor(combined_imputed$debt_relief)
combined_imputed$income_classification_world_bank_2017 <- as.factor(combined_imputed$income_classification_world_bank_2017)

# Define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(combined_imputed)
expl <- expl[-1]

# We now run the GLM Model Function:
after_drop <- gvif_drop(resp, expl, combined_imputed, vif_max=5, glmtype=2, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, combined_imputed, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

step_AIC_mod <- step2.glm(resp, after_drop, combined_imputed, "AIC", Gamma(link="log"), maxit=100)
step_BIC_mod <- step2.glm(resp, after_drop, combined_imputed, "BIC", Gamma(link="log"), maxit=100)
step_AICc_mod <- step2.glm(resp, after_drop, combined_imputed, "AICc", Gamma(link="log"), maxit=100)

summary(step_AIC_mod)
summary(step_BIC_mod)
summary(step_AICc_mod)

par(mfrow = c(2,2))
plot(step_AIC_mod, main="AIC_model")
plot(step_BIC_mod, main="BIC_model")
plot(step_AICc_mod, main="AICc_model")

# selected variables for each model
all.vars(formula(step_AIC_mod)[-1])
# [1] "cost_of_nutrient_adequate_diet_2017_usd_per_day"                                         
# [2] "cost_of_healthy_diet_2017_usd_per_day"                                                   
# [3] "nutrient_adequate_diet_cost_percent_of_average_food_expenditure"                         
# [4] "calorie_sufficient_diet_cost_number_cannot_afford"                                       
# [5] "population_with_access_to_improved_sanitation_y"                                         
# [6] "ratio_of_diabetes_to_overweight_prevalence"                                              
# [7] "deaths_no_access_to_handwashing_facility_sex_both_age_70_number"                         
# [8] "cardiovascular_diseases_ihme_2017"                                                       
# [9] "life_satisfaction_in_cantril_ladder_world_happiness_report_2019"                         
# [10] "income_classification_world_bank_2017"                                                   
# [11] "gdp_growth_per_capita_from_previous_year_2020_q2"                                        
# [12] "income_support"                                                                          
# [13] "nurses_per_1_000_population_oecd"                                                        
# [14] "psychiatrists_per_1_000_population_oecd"                                                 
# [15] "publicly_owned_hospitals_per_million_population_oecd"                                    
# [16] "surgical_specialists_per_1_000_population_oecd"                                          
# [17] "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure"
all.vars(formula(step_BIC_mod)[-1])
# [1] "cost_of_nutrient_adequate_diet_2017_usd_per_day"                                         
# [2] "cost_of_healthy_diet_2017_usd_per_day"                                                   
# [3] "nutrient_adequate_diet_cost_percent_of_average_food_expenditure"                         
# [4] "life_satisfaction_in_cantril_ladder_world_happiness_report_2019"                         
# [5] "income_support"                                                                          
# [6] "nurses_per_1_000_population_oecd"                                                        
# [7] "psychiatrists_per_1_000_population_oecd"                                                 
# [8] "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure"
all.vars(formula(step_AICc_mod)[-1])
# [1] "cost_of_nutrient_adequate_diet_2017_usd_per_day"                                         
# [2] "cost_of_healthy_diet_2017_usd_per_day"                                                   
# [3] "nutrient_adequate_diet_cost_percent_of_average_food_expenditure"                         
# [4] "calorie_sufficient_diet_cost_number_cannot_afford"                                       
# [5] "ratio_of_diabetes_to_overweight_prevalence"                                              
# [6] "deaths_no_access_to_handwashing_facility_sex_both_age_70_number"                         
# [7] "cardiovascular_diseases_ihme_2017"                                                       
# [8] "respiratory_disease_ihme_2017"                                                           
# [9] "life_satisfaction_in_cantril_ladder_world_happiness_report_2019"                         
# [10] "gini_index"                                                                              
# [11] "income_support"                                                                          
# [12] "beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd"              
# [13] "nurses_per_1_000_population_oecd"                                                        
# [14] "psychiatrists_per_1_000_population_oecd"                                                 
# [15] "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure"







