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

# Read in the Random Forest Imputed Datasets

food_rf <- read_csv("R_Scripts/MatthewR/Category Analysis/3. RF/Food/food_rf.csv")
disease_rf <- read_csv("R_Scripts/MatthewR/Category Analysis/3. RF/Disease/disease_rf.csv")
clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")

food_rf <- clean_names(food_rf)
food_rf <- subset(food_rf, select = -1)
disease_rf <- clean_names(disease_rf)
disease_rf <- subset(disease_rf, select = -1)


#Adding the entities back in

entities <- subset(clean_food_water, select = 2)
food_rf_entit <- cbind(entities,food_rf)
diease_fr_entit <- cbind(entities,disease_rf)

random_forest_combined <- merge(food_rf_entit,diease_fr_entit, by = "Entity")
# write.csv(random_forest_combined, file = "random_forest_combined.csv", row.names = TRUE)

random_forest_significant <- subset(random_forest_combined, select = c(1,2, 5, 14, 23, 35, 39, 42, 47))
write.csv(random_forest_significant, file = "random_forest_significant.csv", row.names = TRUE)



















