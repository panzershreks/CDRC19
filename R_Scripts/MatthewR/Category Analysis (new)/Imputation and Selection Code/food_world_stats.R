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
library(corrplot)
library(RColorBrewer)
library(psych)


# We read in the data that we required and we remove the countries which have no response variable value.
# We combine the World Statistics Data with the Food and Water Data

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -c(1,2))
clean_food_water <- clean_food_water[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                        145, 151, 156, 171,173,177,178, 186,193), ]

clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,2,3))
clean_world_stats <- clean_world_stats[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                          145, 151, 156, 171,173,177,178, 186,193),]

clean_combined_f_ws <- cbind(clean_food_water, clean_world_stats)


# We explore the missing values across the datasets.


food_missing_vis <- vis_miss(clean_combined_f_ws, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# ggsave(food_missing_vis, file="food_missing_vis.png", height = 8, width = 8)


# We now impute our missing data using random forests.

set.seed(100)
food_rf <- missForest(as.matrix(clean_combined_f_ws))

# We save our imputed dataset as a dataframe

food_data <- food_rf$ximp
food_data <- as.data.frame.matrix(food_data)

# We write our completed dataframe to a matrix.

# write.csv(food_data,"food_world_stats_complete.csv", row.names = TRUE)


# We now look at the correlation bewteen variables.

Mcor <- food_data
f_ws_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

f_ws_cor_matrix


# We now will use a function we have written to carry out VIF to 
# find what variables we should use in our model.

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

after_drop <- gvif_drop(resp, expl, food_data)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, food_data)
vif(final_model)

step_food <- step(final_model)
summary(step_food)

# We now plot our model assumptions:

par(mfrow = c(2, 2))
plot(step_food)





?step
