library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)
response_variable <- clean_fully_merged[,57]

food_all_missing <- subset(clean_fully_merged, select = c(221:237))
world_stats_all_missing <- subset(clean_fully_merged, select = c(350))
food_missing_no_res <- cbind(food_all_missing, world_stats_all_missing)



# impute
food_rf <- missForest(data.frame(food_missing_no_res))
food_rf_df <- as.data.frame(food_rf$ximp)
any(is.na(food_rf_df)) # check that no NAs
write.csv(food_rf_df, file="GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV/food_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_food <- cbind(response_variable, food_rf_df)

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

















