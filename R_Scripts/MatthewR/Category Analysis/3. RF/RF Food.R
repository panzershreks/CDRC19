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


clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -c(1,2))

set.seed(100)
food_rf <- missForest(as.matrix(clean_food_water))

food_data <- food_rf$ximp
food_data <- as.data.frame.matrix(food_data)
# write.csv(food_data,"food_rf.csv", row.names = TRUE)

full_model_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     cost_of_calorie_sufficient_diet_2017_usd_per_day + 
                     cost_of_nutrient_adequate_diet_2017_usd_per_day +  
                     cost_of_healthy_diet_2017_usd_per_day +  
                     calorie_sufficient_diet_cost_percent_of_1_20_poverty_line +  
                     nutrient_adequate_diet_cost_percent_of_1_20_poverty_line + 
                     healthy_diet_cost_percent_of_1_20_poverty_line +  
                     calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
                     nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
                     healthy_diet_cost_percent_of_average_food_expenditure +  
                     calorie_sufficient_diet_cost_percent_cannot_afford +  
                     nutrient_adequate_diet_cost_percent_cannot_afford +  
                     healthy_diet_cost_percent_cannot_afford +  
                     calorie_sufficient_diet_cost_number_cannot_afford +  
                     nutrient_adequate_diet_cost_number_cannot_afford +  
                     healthy_diet_cost_number_cannot_afford +  
                     population_with_access_to_improved_sanitation_y +
                     population_without_access_to_improved_sanitation_y, data = food_data)
vif(full_model_1)

# Remove healthy_diet_cost_percent_of_1_20_poverty_line

f_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     cost_of_calorie_sufficient_diet_2017_usd_per_day + 
                     cost_of_nutrient_adequate_diet_2017_usd_per_day +  
                     cost_of_healthy_diet_2017_usd_per_day +  
                     calorie_sufficient_diet_cost_percent_of_1_20_poverty_line +  
                     nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
                     calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
                     nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
                     healthy_diet_cost_percent_of_average_food_expenditure +  
                     calorie_sufficient_diet_cost_percent_cannot_afford +  
                     nutrient_adequate_diet_cost_percent_cannot_afford +  
                     healthy_diet_cost_percent_cannot_afford +  
                     calorie_sufficient_diet_cost_number_cannot_afford +  
                     nutrient_adequate_diet_cost_number_cannot_afford +  
                     healthy_diet_cost_number_cannot_afford +  
                     population_with_access_to_improved_sanitation_y +
                     population_without_access_to_improved_sanitation_y, data = food_data)
vif(f_1)

# Remove cost_of_nutrient_adequate_diet_2017_usd_per_day

f_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            calorie_sufficient_diet_cost_percent_of_1_20_poverty_line +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            healthy_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            nutrient_adequate_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            healthy_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y +
            population_without_access_to_improved_sanitation_y, data = food_data)
vif(f_2)

# Remove calorie_sufficient_diet_cost_percent_of_1_20_poverty_line

f_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            healthy_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            nutrient_adequate_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            healthy_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y +
            population_without_access_to_improved_sanitation_y, data = food_data)
vif(f_3)

# Remove healthy_diet_cost_number_cannot_afford

f_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            healthy_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            nutrient_adequate_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y +
            population_without_access_to_improved_sanitation_y, data = food_data)
vif(f_4)

# Remove healthy_diet_cost_percent_of_average_food_expenditure

f_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            nutrient_adequate_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y +
            population_without_access_to_improved_sanitation_y, data = food_data)
vif(f_5)

# Remove population_without_access_to_improved_sanitation_y 

f_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            nutrient_adequate_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y, data = food_data)
vif(f_6)


# Remove nutrient_adequate_diet_cost_percent_cannot_afford

f_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line +  
            calorie_sufficient_diet_cost_percent_of_average_food_expenditure + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y, data = food_data)
vif(f_7)


# Remove calorie_sufficient_diet_cost_percent_of_average_food_expenditure

f_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
            cost_of_calorie_sufficient_diet_2017_usd_per_day +  
            cost_of_healthy_diet_2017_usd_per_day +  
            nutrient_adequate_diet_cost_percent_of_1_20_poverty_line + 
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +  
            calorie_sufficient_diet_cost_percent_cannot_afford +  
            healthy_diet_cost_percent_cannot_afford +  
            calorie_sufficient_diet_cost_number_cannot_afford +  
            nutrient_adequate_diet_cost_number_cannot_afford +  
            population_with_access_to_improved_sanitation_y, data = food_data)
vif(f_8)

step_food <- step(f_8)
summary(step_food)




