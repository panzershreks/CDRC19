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


clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -1)

# We look at the missing data and export the missing data table to a CSV for making into latex.

missing_table <- miss_var_summary(clean_food_water, sort_miss = TRUE)
vis_miss(clean_food_water)
missing_table

# write.csv(missing_table, file = "f_w_missing.csv", row.names = TRUE)


# We now want to do imputation

set.seed(100)
food_water_imputation <- mice(data = clean_food_water, m = 5, method = c("cart"), maxit = 100)

food_water_imputation$loggedEvents

# We have that there are a few constant/collinear columns, so we will now deal with them
# by removing them.

food_water_1 <- complete(food_water_imputation, 1)
food_water_2 <- complete(food_water_imputation, 2)
food_water_3 <- complete(food_water_imputation, 3)
food_water_4 <- complete(food_water_imputation, 4)
food_water_5 <- complete(food_water_imputation, 5)

# now to proceed we will remove columns but we will from now on work with only
# food_water_1 data - 6,7,8 are collinear columns.

working_food <- food_water_1

working_food <- subset(working_food, select = -c(6,7,8))

# write.csv(working_food,"working_food.csv", row.names = TRUE)

# There is now no missing data:

miss_var_summary(working_food)

# Now we want to look at the correlation between the variables.

#numeric_variables <- working_food[]

Mcor <- working_food[,2:16]

f_w_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

#ggsave(f_w_cor_matrix, file="f_w_cor_matrix.png")
# screenshotted it as it didn't work on the output.

cor_data_frame <- round(cor(Mcor),2)
# write.csv(cor_data_frame,"food_wat_correlation.csv", row.names = TRUE)

# write up observations about this.



# Now you want to use VIF - we will remove the largest valued variable each time
# until we have that all the values are under five.

full_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
                 cost_of_healthy_diet_2017_usd_per_day + calorie_sufficient_diet_cost_percent_of_average_food_expenditure +
                 nutrient_adequate_diet_cost_percent_of_average_food_expenditure + healthy_diet_cost_percent_of_average_food_expenditure +
                 calorie_sufficient_diet_cost_percent_cannot_afford + nutrient_adequate_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
                 calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
                 healthy_diet_cost_number_cannot_afford + population_with_access_to_improved_sanitation_y +
                 population_without_access_to_improved_sanitation_y, data = working_food)

vif(full_model)

# We then remove healthy_diet_cost_number_cannot_afford

fw1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
                   cost_of_healthy_diet_2017_usd_per_day + calorie_sufficient_diet_cost_percent_of_average_food_expenditure +
                   nutrient_adequate_diet_cost_percent_of_average_food_expenditure + healthy_diet_cost_percent_of_average_food_expenditure +
                   calorie_sufficient_diet_cost_percent_cannot_afford + nutrient_adequate_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
                   calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
                   population_with_access_to_improved_sanitation_y +
                   population_without_access_to_improved_sanitation_y, data = working_food)

vif(fw1)

# We then remove population_without_access_to_improved_sanitation_y

fw2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
            cost_of_healthy_diet_2017_usd_per_day + calorie_sufficient_diet_cost_percent_of_average_food_expenditure +
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure + healthy_diet_cost_percent_of_average_food_expenditure +
            calorie_sufficient_diet_cost_percent_cannot_afford + nutrient_adequate_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
            calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
            population_with_access_to_improved_sanitation_y, data = working_food)

vif(fw2)

# We then remove healthy_diet_cost_percent_of_average_food_expenditure 

fw3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
            cost_of_healthy_diet_2017_usd_per_day + calorie_sufficient_diet_cost_percent_of_average_food_expenditure +
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +
            calorie_sufficient_diet_cost_percent_cannot_afford + nutrient_adequate_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
            calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
            population_with_access_to_improved_sanitation_y, data = working_food)

vif(fw3)

# We then remove nutrient_adequate_diet_cost_percent_cannot_afford 

fw4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
            cost_of_healthy_diet_2017_usd_per_day + calorie_sufficient_diet_cost_percent_of_average_food_expenditure +
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +
            calorie_sufficient_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
            calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
            population_with_access_to_improved_sanitation_y, data = working_food)

vif(fw4)

# We then remove calorie_sufficient_diet_cost_percent_of_average_food_expenditure

fw5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ cost_of_calorie_sufficient_diet_2017_usd_per_day + cost_of_nutrient_adequate_diet_2017_usd_per_day +
            cost_of_healthy_diet_2017_usd_per_day +
            nutrient_adequate_diet_cost_percent_of_average_food_expenditure +
            calorie_sufficient_diet_cost_percent_cannot_afford + healthy_diet_cost_percent_cannot_afford +
            calorie_sufficient_diet_cost_number_cannot_afford + nutrient_adequate_diet_cost_number_cannot_afford +
            population_with_access_to_improved_sanitation_y, data = working_food)

vif(fw5)

# We now have all the values under five, so we can say all these variables deserve to be in the model.

# Now we will ues the step function to find significant variables.

step_fw5 <- step(fw5)
summary(step_fw5)
plot(step_fw5)

