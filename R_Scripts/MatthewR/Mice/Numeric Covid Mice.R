# Imputing all the numeric variables MICE.

library("ggplot2")
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library("mice")

fully_merged_df <- read_csv("Combined DataFrame Work/fully_merged_df.csv")
fully_merged_df <- subset(fully_merged_df, select = -1)
fully_merged_df <- clean_names(fully_merged_df)

# Run the factor variable code first to make sure we have the factors!

numeric_covid_data <- subset(fully_merged_df, select = -c(2:12, 14:19, 21:23))
factor_covid_data <- subset(fully_merged_df, select = c(2:12, 14:19, 21:23))

# We are now going to use MICE to imputate the numeric variables to complete it.

numeric_imp <- mice(data = numeric_covid_data, m = 1, method = "cart", maxit = 5)


# with maxit = 3 it imputed data to the first three columns, then with maxit = 5 it did the
# same but for the first five columns. For five columns this took about an hour, so 
# not really sure how we're going to manage to do this for 352 columns? 
# Ask Simon about this.

test_big_imp_df <- complete(numeric_imp, 1)

write.csv(test_big_imp_df,"imp_maxit_5.csv", row.names = TRUE)




missing_data_summary <- miss_var_summary(test_big_imp_df)
missing_data_summary








