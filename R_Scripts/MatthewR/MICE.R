# Taking a look at the MCIE package
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

# Now let's try this with COVID Data, read it in from CSV file.

fully_merged_df <- read_csv("Combined DataFrame Work/fully_merged_df.csv")

# remove the columns of numbers from CSV file.

fully_merged_df <- subset(fully_merged_df, select = -1)

# The MICE function does not like having spaces in the column name,
# so we use this function to replace all of the spaces with _

fully_merged_df <- clean_names(fully_merged_df)
?step

test_mean_df <- subset(fully_merged_df, select = c(1,13,54,58))
test_mice_df <- subset(fully_merged_df, select = c(1,13,54,58))

summary(test_mean_df)
summary(test_mice_df)

# Now we do mean imputation

test_mean_df$stringency_index[which(is.na(test_mean_df$stringency_index))] = mean(test_mean_df$stringency_index, na.rm = TRUE)
test_mean_df$total_confirmed_deaths_due_to_covid_19[which(is.na(test_mean_df$total_confirmed_deaths_due_to_covid_19))] = mean(test_mean_df$total_confirmed_deaths_due_to_covid_19, na.rm = TRUE)
test_mean_df$total_confirmed_deaths_due_to_covid_19_per_million_people[which(is.na(test_mean_df$total_confirmed_deaths_due_to_covid_19_per_million_people))] = mean(test_mean_df$total_confirmed_deaths_due_to_covid_19_per_million_people, na.rm = TRUE)


# Now to try MICE

covid_imputation <- mice(data = test_mice_df, m = 5, method = c("","pmm","pmm","pmm"), maxit = 100)

?mice

covid_imputation$imp$stringency_index



covid_mice_df1 <- complete(covid_imputation, 1)
covid_mice_df2 <- complete(covid_imputation, 2)
covid_mice_df3 <- complete(covid_imputation, 3)
covid_mice_df4 <- complete(covid_imputation, 4)
covid_mice_df5 <- complete(covid_imputation, 5)


ggplot(data = covid_mice_df1, aes(x=total_confirmed_deaths_due_to_covid_19_per_million_people, y = stringency_index)) + geom_point() + geom_smooth(method="lm")

# I think at this stage we then take all five imputed datasets and make models off of them
# and then essentially work with these and gather thoughts based on all of the models.

#https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/

# Let's try this for modelling the death rates per populatiuon against the stringency index.

fitm <- with(covid_imputation, lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index  ))

summary(fitm)

summary(pool(fitm))

# this shows that these things aren't very correlated/there is not a significant relationship.




