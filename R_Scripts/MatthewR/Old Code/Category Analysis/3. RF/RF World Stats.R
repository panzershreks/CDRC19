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

# won't work for this category.

clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,2))

# Rows with fully NA values:
# take_rows_away <- clean_world_stats[-c(49,55,68,89,106,119,125,131,143,144,145,152,157,172,174,178,187,194), ] 


world_stats_rf <- missForest(as.matrix(clean_world_stats))

world_stats_data <- world_stats_rf$ximp






