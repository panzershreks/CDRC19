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

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -1)

missing_table <- miss_var_summary(clean_disease, sort_miss = TRUE)
vis_miss(clean_disease)
missing_table
# write.csv(missing_table, file = "disease_missing.csv", row.names = TRUE)

# We now want to do imputation

set.seed(100)
disease_imputation <- mice(data = clean_disease, m = 5, method = c("cart"), maxit = 100)

disease_imputation$loggedEvents

# We have that there are a few constant/collinear columns, so we will now deal with them
# by removing them.

disease_1 <- complete(disease_imputation, 1)
disease_2 <- complete(disease_imputation, 2)
disease_3 <- complete(disease_imputation, 3)
disease_4 <- complete(disease_imputation, 4)
disease_5 <- complete(disease_imputation, 5)

# we will from now on work with only the first imputed version of events.

working_disease <- disease_1

# There is still some missing data -

miss_var_summary(working_disease)
vis_miss(working_disease)
nrow(working_disease)
working_disease <- na.omit(working_disease)

# now do correlation between values
# then VIF
# then model.























