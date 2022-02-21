#### Attempt at using missForest but without the response variable

library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)
library(datasets)
library(car)

combined_all_missing <- read_csv("GLM Data and Analysis/Combined CSV/combined_all_missing.csv")
combined_all_missing <- clean_names(combined_all_missing)
combined_all_missing <- subset(combined_all_missing, select = -1)

combined_all_missing$income_support <- as.factor(combined_all_missing$income_support)
combined_all_missing$debt_relief <- as.factor(combined_all_missing$debt_relief)
combined_all_missing$income_classification_world_bank_2017 <- as.factor(combined_all_missing$income_classification_world_bank_2017)

# we now remove the response variable

combined_all_missing <- subset(combined_all_missing, select = -1)

# We now use random forests to impute the missing data

set.seed(100)
combined_imputed_rf <- missForest(as.data.frame(combined_all_missing))
combined_imputed <- as.data.frame.matrix(combined_imputed_rf$ximp)


write.csv(combined_imputed,"R_Scripts//MatthewR/new missForest//new_rf_combined.csv", row.names = TRUE)



