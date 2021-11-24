# Taking a look at the MCIE package
library('mice')
library(tidyverse)
library(dplyr)


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

# Now let's try this with COVID Data, read it in from CSV file.

fully_merged_df <- read_csv("Combined DataFrame Work/fully_merged_df.csv")

# remove the columns of numbers from CSV file.

fully_merged_df <- subset(fully_merged_df, select = -1)

test_mean_df <- subset(fully_merged_df, select = c(1,13,54,58))
test_mice_df <- subset(fully_merged_df, select = c(1,13,54,58))

summary(test_mean_df)
summary(test_mice_df)

# Now we do mean imputation

test_mean_df$stringency_index[which(is.na(test_mean_df$stringency_index))] = mean(test_mean_df$stringency_index, na.rm = TRUE)
test_mean_df$`Total confirmed deaths due to COVID-19`[which(is.na(test_mean_df$`Total confirmed deaths due to COVID-19`))] = mean(test_mean_df$`Total confirmed deaths due to COVID-19`, na.rm = TRUE)
test_mean_df$`Total confirmed deaths due to COVID-19 per million people`[which(is.na(test_mean_df$`Total confirmed deaths due to COVID-19 per million people`))] = mean(test_mean_df$`Total confirmed deaths due to COVID-19 per million people`, na.rm = TRUE)


# Now to try MICE - but this doesn't work...

covid_imputation <- mice(data = test_mice_df, m = 5, method = c("","pmm","pmm","pmm"), maxit = 20)

