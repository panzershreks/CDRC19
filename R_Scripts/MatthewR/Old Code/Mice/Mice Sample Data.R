# Mice Example

library('mice')
library(tidyverse)
library(dplyr)
library("janitor")
library(ggplot2)

# Create a pretend dataset
Age <- c(12,14,15,16,17,12,15,16,17,18,19,19,18,16,17,18)
BMI <- c(15,16,17,NA,14,22,14,NA,14,13,12,15,16,17,21,30)
Score <- c(NA,4,NA,6,7,1,2,NA,8,9,10,NA,4,2,3,4)

test_df <- data.frame(Age, BMI, Score)

summary(test_df)

# This is simple imputation which has been learned from YouTube.
# This line of code adds the mean of the BMI column to all the missing values.

test_df$BMI[which(is.na(test_df$BMI))] = mean(test_df$BMI, na.rm = TRUE)
test_df$Score[which(is.na(test_df$Score))] = mean(test_df$Score, na.rm = TRUE)

# Mice imputation

test_df_2 <- data.frame(Age, BMI, Score)

imputation <- mice(data = test_df_2, m = 5, method = c(" ","pmm","pmm"), maxit = 20)

summary(test_df_2$BMI)
imputation$imp$BMI


mice_df <- complete(imputation, 5)