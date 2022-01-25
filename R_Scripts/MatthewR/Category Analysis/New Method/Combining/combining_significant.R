# Combining the dataframes

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


# We read in the data that we require.

# World Statistics

world_stats_1 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/World Stats Imputed Data/world_stats_1.csv")
world_stats_1 <- clean_names(world_stats_1)
world_stats_1 <- subset(world_stats_1, select = -1)

world_stats_2 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/World Stats Imputed Data/world_stats_2.csv")
world_stats_2 <- clean_names(world_stats_2)
world_stats_2 <- subset(world_stats_2, select = -1)

world_stats_3 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/World Stats Imputed Data/world_stats_3.csv")
world_stats_3 <- clean_names(world_stats_3)
world_stats_3 <- subset(world_stats_3, select = -1)


world_stats_4 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/World Stats Imputed Data/world_stats_4.csv")
world_stats_4 <- clean_names(world_stats_4)
world_stats_4 <- subset(world_stats_4, select = -1)

world_stats_5 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/World Stats Imputed Data/world_stats_5.csv")
world_stats_5 <- clean_names(world_stats_5)
world_stats_5 <- subset(world_stats_5, select = -1)


food_1 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Food and Water Imputed/food_water_1.csv")
food_1 <- clean_names(food_1)
food_1 <- subset(food_1, select = -1)

food_2 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Food and Water Imputed/food_water_2.csv")
food_2 <- clean_names(food_2)
food_2 <- subset(food_2, select = -1)

food_3 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Food and Water Imputed/food_water_3.csv")
food_3 <- clean_names(food_3)
food_3 <- subset(food_3, select = -1)

food_4 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Food and Water Imputed/food_water_4.csv")
food_4 <- clean_names(food_4)
food_4 <- subset(food_4, select = -1)

food_5 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Food and Water Imputed/food_water_5.csv")
food_5 <- clean_names(food_5)
food_5 <- subset(food_5, select = -1)


disease_1 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Disease Imputed/disease_1.csv")
disease_1 <- clean_names(disease_1)
disease_1 <- subset(disease_1, select = -1)

disease_2 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Disease Imputed/disease_2.csv")
disease_2 <- clean_names(disease_2)
disease_2 <- subset(disease_2, select = -1)

disease_3 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Disease Imputed/disease_3.csv")
disease_3 <- clean_names(disease_3)
disease_3 <- subset(disease_3, select = -1)

disease_4 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Disease Imputed/disease_4.csv")
disease_4 <- clean_names(disease_4)
disease_4 <- subset(disease_4, select = -1)

disease_5 <- read_csv("R_Scripts/MatthewR/Category Analysis/New Method/Combining/Disease Imputed/disease_5.csv")
disease_5 <- clean_names(disease_5)
disease_5 <- subset(disease_5, select = -1)


# now want to combine them all and then select only sig. ones.



































