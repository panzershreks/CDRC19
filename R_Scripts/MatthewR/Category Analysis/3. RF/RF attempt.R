# Trying random forests
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


# Learning how RF works.
Name <- c(1:100)
Age <- c(3:102)
Height <- c(1:99,NA)
df <- data.frame(Name, Age, Height)
missForest(df,ntree = 100)

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -c(1,2))

set.seed(100)
missForest(clean_food_water, ntree = 100,xtrue = TRUE)



?missForest


