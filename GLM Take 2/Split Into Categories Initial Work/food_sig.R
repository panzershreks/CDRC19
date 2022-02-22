library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

# import column names we want to keep
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]

# import data
combined_all_missing <- read_csv("GLM Data and Analysis/Combined CSV/combined_all_missing.csv")
combined_all_missing <- clean_names(combined_all_missing)
combined_all_missing <- subset(combined_all_missing, select = -1)

# now subset disease variables and response
# response = column 1
# disease = 19 -> 46
# food/water = 2 -> 18 + world stats = 47

food_missing <- subset(combined_all_missing, select = c(1:18, 47))

# remove response for imputation
covid_deaths <- subset(food_missing, select=1)
food_missing_no_res <- subset(food_missing, select=-1)

# impute
food_rf <- missForest(data.frame(food_missing_no_res))
food_rf_df <- as.data.frame(food_rf$ximp)
any(is.na(food_rf_df)) # check that no NAs
write.csv(food_rf_df, file="GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV/food_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_food <- cbind(covid_deaths, food_rf_df)

# remove junk vars
keep_index <- which(colnames(full_imputed_food) %in% keep_cols)
subset_food <- subset(full_imputed_food, select=keep_index)

# drop by VIF
resp <- colnames(subset_food)[1]
expl <- colnames(subset_food)[-1]
after_drop <- gvif_drop(resp, expl, subset_food, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_food, family=Gamma(link="log"))
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, subset_food, "AICc", Gamma(link="log"), maxit=100)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), subset_food$total_confirmed_deaths_due_to_covid_19_per_million_people)

# save CSVs
sig_vars <- all.vars(formula(step_drop_vif)[-1])
sig_food_imputed <- subset(full_imputed_food, select=sig_vars)
sig_food_missing <- subset(food_missing_no_res, select=sig_vars)
write.csv(sig_food_imputed, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_food_imputed.csv", row.names=FALSE)
write.csv(sig_food_missing, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_food_missing.csv", row.names=FALSE)

















