# practicing with GLM

library(readr)
clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,2))
clean_world_stats <- na.omit(clean_world_stats)

# We are now going to use the GLM function

fit_test <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
      life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = clean_world_stats,
    family = inverse.gaussian)

par(mfrow = c(2, 2))
plot(fit_test)



# Trying again

food_world_stats_complete <- read_csv("R_Scripts/MatthewR/Category Analysis (new)/CSV Files/Holding/food_world_stats_complete.csv")
food_world_stats_complete <- subset(food_world_stats_complete, select = -1)


# We are now going to use the old VIF function and LM to check for collinearity.

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
          "population_without_access_to_improved_sanitation_y", 
          "life_satisfaction_in_cantril_ladder_world_happiness_report_2019")

after_drop <- gvif_drop(resp, expl, food_world_stats_complete)
after_drop

# now we will try GLM with these variables outputted by the VIF.

test_model <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                    cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day + 
                    healthy_diet_cost_percent_of_1_20_poverty_line + nutrient_adequate_diet_cost_percent_of_average_food_expenditure +
                    calorie_sufficient_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford + 
                    calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
                    population_with_access_to_improved_sanitation_y + 
                    life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = food_world_stats_complete, family =  quasipoisson(link = "log"))

plot((test_model))


AIC(test_model)
step_test <- step(test_model)

# this doesn't work. So we will try it with the selected variables from food category
# I'll take these variables as a subset from my food+disease sig vars.


matthew_sig_var_complete <- read_csv("LM Data and Analysis/Categories Complete/matthew_sig_var_complete.csv")
food_for_model <- subset(matthew_sig_var_complete, select = c(2,3,4,5,6))

food_test_model <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                         healthy_diet_cost_percent_cannot_afford + 
                         cost_of_calorie_sufficient_diet_2017_usd_per_day + healthy_diet_cost_percent_of_1_20_poverty_line + 
                         life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = food_for_model, family = quasipoisson(link = "log"))


par(mfrow = c(2, 2))
plot(food_test_model)



disease_data_for_model <- subset(matthew_sig_var_complete, select = c(2,7,8,9,10,11,12))
disease_test_model <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                            age_standardised_diabetes_prevalence_male + 
                            cardiovascular_diseases_ihme_2017 + 
                            meningitis_ihme_2017+prevalence_of_obesity_female_who_2019 + 
                            kidney_disease_ihme_2017 + diabetes_blood_and_endocrine_disease_ihme_2017, data = disease_data_for_model, family = quasipoisson(link = "log"))


par(mfrow = c(2, 2))
plot(disease_test_model)



# We now want to test GLM on the missing final data.
# read in the missing but now imputed data using random forest.

all_cat_imputed_now_imputed <- read_csv("LM Data and Analysis/Categories with Missing/all_cat_imputed_now_imputed.csv")








