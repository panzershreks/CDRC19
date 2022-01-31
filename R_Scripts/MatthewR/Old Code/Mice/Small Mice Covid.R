# Using Mice for a small subset of Covid Data

library('mice')
library(tidyverse)
library(dplyr)
library("janitor")
library(ggplot2)

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



