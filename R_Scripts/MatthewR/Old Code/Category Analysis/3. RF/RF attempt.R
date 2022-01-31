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



# Now we are going to try Random Forests for the World Stats Data

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -c(1,2))

# no_rows_missing <- clean_disease[-c(56,126), ] 

missForest(as.matrix(clean_disease))












