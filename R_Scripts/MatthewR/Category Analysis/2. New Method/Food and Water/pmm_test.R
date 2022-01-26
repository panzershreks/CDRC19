# PMM test


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

# We read in and tidy the data we require.

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -1)

# We look at the missing data and visualise it.

missing_table <- miss_var_summary(clean_food_water, sort_miss = TRUE)
food_missing_vis <- vis_miss(clean_food_water, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# ggsave(food_missing_vis, file="food_missing_vis.png", height = 8 )

# We now carry out MICE Imputation, and we use the PMM method.

# The PMM method does not work in this case, and a suggested online way to get around it is to use the CART method, 
# so we use that instead.

set.seed(100)
food_water_imputation <- mice(data = clean_food_water, m = 5, method = c("pmm"), maxit = 100)