library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)
library(readr)
library(ggplot2)
library(naniar)
library(visdat)
library(missForest)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

set.seed(100)

clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)
response_variable <- clean_fully_merged[,57]

healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")
healthcare_all_mis <- clean_names(healthcare_all_mis)
clean_healthcare_no_res <- subset(healthcare_all_mis, select = -c(1,2,3))


# impute
healthcare_rf <- missForest(data.frame(clean_healthcare_no_res))
healthcare_rf_df <- as.data.frame(healthcare_rf$ximp)
any(is.na(healthcare_rf_df)) # check that no NAs
write.csv(healthcare_rf_df, file="GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV/healthcare_full_imputed.csv", row.names=FALSE)

# insert response column
full_imputed_healthcare <- cbind(response_variable, healthcare_rf_df)

# import column names we want to keep
keep_cols <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]

# remove junk vars
keep_index <- which(colnames(full_imputed_healthcare) %in% keep_cols)
subset_healthcare <- subset(full_imputed_healthcare, select=keep_index)

# drop by VIF
resp <- colnames(subset_healthcare)[1]
expl <- colnames(subset_healthcare)[-1]
after_drop <- gvif_drop(resp, expl, subset_healthcare, vif_max=5)
drop_vif_formula <- lm_formula_paster(resp, after_drop)
model_drop_vif <- glm(drop_vif_formula, subset_healthcare, family=Gamma(link="log"))
vif(model_drop_vif)

# backwards selection using AICc
step_drop_vif <- step2.glm(resp, after_drop, subset_healthcare, "AICc", Gamma(link="log"), maxit=100)
summary(step_drop_vif)

# plots
par(mfrow = c(2, 2))
plot(step_drop_vif)
par(mfrow = c(1, 1))
plot(fitted(step_drop_vif), subset_healthcare$total_confirmed_deaths_due_to_covid_19_per_million_people)

# save CSVs
sig_vars <- all.vars(formula(step_drop_vif)[-1])
sig_healthcare_imputed <- subset(full_imputed_healthcare, select=sig_vars)
sig_healthcare_missing <- subset(clean_healthcare_no_res, select=sig_vars)
write.csv(sig_healthcare_imputed, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV/sig_healthcare_imputed.csv", row.names=FALSE)
write.csv(sig_healthcare_missing, file="GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV/sig_healthcare_missing.csv", row.names=FALSE)





















