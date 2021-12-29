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

















