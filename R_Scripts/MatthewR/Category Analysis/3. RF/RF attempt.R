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


# Learning how RF works.
Name <- c(1:100)
Age <- c(3:102)
Height <- c(1:99,NA)
df <- data.frame(Name, Age, Height)
missForest(df,ntree = 100)









