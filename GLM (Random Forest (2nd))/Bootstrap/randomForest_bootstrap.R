library(readr)
library("janitor")
library(randomForest)
library(MuMIn)
library(glm2)
library(dplyr)
library(bannerCommenter)
library(tidyr)
library(tictoc)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")
set.seed(100)

############################################################################
############################################################################
###                                                                      ###
###                       1. LOAD DATA BY CATEGORY                       ###
###                                                                      ###
############################################################################
############################################################################
clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)

disease_all_missing <- subset(clean_fully_merged, select = c(144:171))
food_all_missing <- subset(clean_fully_merged, select = c(221:237))
world_stats_all_missing <- subset(clean_fully_merged, select = c(350))

# Now we do this for the response variable.
# The repsonse variable is # 57

response_variable <- clean_fully_merged[,57]

economic_all_missing <- subset(clean_fully_merged, select = c(238:273))
names(economic_all_missing)[names(economic_all_missing) == 'mean'] <- 'mean_monthly_income'
economic_all_missing$gdp_growth_per_capita_from_previous_year_2020_q2 <- with(economic_all_missing, gdp_growth_from_previous_year_2020_q2 / pop2021)
to_drop <- c("pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
             "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
             "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
             "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y",
             "gdp_per_capita_ppp_2011_wdi_2016", "gdp", "gdp_growth_from_previous_year_2020_q2",
             "pop2021", "gini_coefficient_world_bank_2016", 
             "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017")

economic_all_missing = economic_all_missing[,!(names(economic_all_missing)%in% to_drop)]
economic_all_missing$income_classification_world_bank_2017 <- as.factor(economic_all_missing$income_classification_world_bank_2017)
covid_all_missing <- read_csv("GLM Data and Analysis/Category CSV/covid_all_missing.csv")
covid_all_missing <- clean_names(covid_all_missing)
covid_all_missing <- subset(covid_all_missing, select = -1)
covid_all_missing$income_support <- as.factor(covid_all_missing$income_support)
covid_all_missing <- subset(covid_all_missing, select=-debt_relief)

healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")
healthcare_all_mis <- clean_names(healthcare_all_mis)
healthcare_all_mis <- subset(healthcare_all_mis, select = -c(1,2,3))

demographic_all_missing <- read_csv("GLM Data and Analysis/Category CSV/demographic_all_missing.csv")
demographic_all_missing <- clean_names(demographic_all_missing)
demographic_all_missing <- subset(demographic_all_missing, select=-total_population_gapminder_hyde_un)

enviroment_all_missing <- read_csv("GLM Data and Analysis/Category CSV/enviroment_all_missing.csv")
enviroment_all_missing <- clean_names(enviroment_all_missing)
enviroment_all_missing <- subset(enviroment_all_missing, select = -1)

food_all_missing <- cbind(food_all_missing, world_stats_all_missing)


############################################################################
############################################################################
###                                                                      ###
###                             2. BOOTSTRAP                             ###
###                                                                      ###
############################################################################
############################################################################

n_bootstrap_samples <- 1000
randomForest_ful_included_vars_bootstrap <- c()
randomForest_imp_included_vars_bootstrap <- c()
randomForest_mis_included_vars_bootstrap <- c()
keep_cols_all <- colnames(read_csv(file = "GLM Take 2/Combined Model/subset_of_total.csv"))[-1]

cat_combined_mis <- cbind(response_variable, covid_all_missing, disease_all_missing, economic_all_missing,
                          enviroment_all_missing, food_all_missing, healthcare_all_mis, demographic_all_missing)

tic("Bootstrap")
for (bootstrap_iteration in 1:n_bootstrap_samples) {
  ##-----------------------
  ##  Bootstrap Samples  --
  ##-----------------------
  bootstrap_sample_full <- cat_combined_mis[sample(nrow(cat_combined_mis), nrow(cat_combined_mis)), ]
  bootstrap_sample_no_res <- subset(bootstrap_sample_full, select=-total_confirmed_deaths_due_to_covid_19_per_million_people)
  bootstrap_response <- subset(bootstrap_sample_full, select=total_confirmed_deaths_due_to_covid_19_per_million_people)
  
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###                                                                      :::
  ###                Full Model                                            :::
  ###                                                                      :::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Impute Bootstrap Sample
  total_confirmed_deaths_due_to_covid_19_per_million_people <- bootstrap_response$total_confirmed_deaths_due_to_covid_19_per_million_people
  full_model_df <- rfImpute(x=as.data.frame(bootstrap_sample_no_res), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  # Remove Unwanted Vars
  keep_index_full <- which(colnames(full_model_df) %in% keep_cols_all)
  subset_full <- subset(full_model_df, select=keep_index_full)
  # Model Selection
  resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
  expl <- colnames(subset_full)[-1]
  after_drop <- gvif_drop(resp, expl, subset_full, vif_max=5, glmtype=2, maxit=100)
  step_full_mod <- step2.glm(resp, after_drop, subset_full, "AICc", Gamma(link="log"), maxit=100)
  randomForest_included_vars_full <- all.vars(formula(step_full_mod)[-1])
  randomForest_ful_included_vars_bootstrap <- c(randomForest_ful_included_vars_bootstrap, randomForest_included_vars_full)
  print(paste0(bootstrap_iteration, " - FULL MODEL COMPLETE"))
  
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###                                                                      :::
  ###                Models By Category (Imputed & Missing)                :::
  ###                                                                      :::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ###:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  print("Category Models")
  ##----------------------------------------
  ##  Split Bootstrap Sample By Category  --
  ##----------------------------------------
  # Column Names per Category
  covid_colnames <- colnames(covid_all_missing)
  disease_colnames <- colnames(disease_all_missing)
  econ_colnames <- colnames(economic_all_missing)
  environment_colnames <- colnames(enviroment_all_missing)
  food_colnames <- colnames(food_all_missing)
  healthcare_colnames <- colnames(healthcare_all_mis)
  demographic_colnames <- colnames(demographic_all_missing)
  # Index of Columns per Category
  keep_index_covid <- which(colnames(bootstrap_sample_no_res) %in% covid_colnames)
  keep_index_disease <- which(colnames(bootstrap_sample_no_res) %in% disease_colnames)
  keep_index_econ <- which(colnames(bootstrap_sample_no_res) %in% econ_colnames)
  keep_index_environment <- which(colnames(bootstrap_sample_no_res) %in% environment_colnames)
  keep_index_food <- which(colnames(bootstrap_sample_no_res) %in% food_colnames)
  keep_index_healthcare <- which(colnames(bootstrap_sample_no_res) %in% healthcare_colnames)
  keep_index_demographic <- which(colnames(bootstrap_sample_no_res) %in% demographic_colnames)
  # Bootstrap Samples by Category
  subset_covid <- subset(bootstrap_sample_no_res, select=keep_index_covid)
  subset_disease <- subset(bootstrap_sample_no_res, select=keep_index_disease)
  subset_econ <- subset(bootstrap_sample_no_res, select=keep_index_econ)
  subset_environment <- subset(bootstrap_sample_no_res, select=keep_index_environment)
  subset_food <- subset(bootstrap_sample_no_res, select=keep_index_food)
  subset_healthcare <- subset(bootstrap_sample_no_res, select=keep_index_healthcare)
  subset_demographic <- subset(bootstrap_sample_no_res, select=keep_index_demographic)
  
  
  ##------------------------
  ##  Impute by Category  --
  ##------------------------
  print(paste0(bootstrap_iteration, " - IMPUTE COVID DATA..."))
  covid_full_imp <- rfImpute(x=as.data.frame(subset_covid), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  print(paste0(bootstrap_iteration, " - IMPUTE DISEASE DATA..."))
  disease_full_imp <- rfImpute(x=as.data.frame(subset_disease), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  print(paste0(bootstrap_iteration, " - IMPUTE ECON DATA..."))
  econ_full_imp <- rfImpute(x=as.data.frame(subset_econ), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  environment_full_imp <- cbind(bootstrap_response, subset_environment)
  print(paste0(bootstrap_iteration, " - IMPUTE FOOD DATA..."))
  food_full_imp <- rfImpute(x=as.data.frame(subset_food), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  print(paste0(bootstrap_iteration, " - IMPUTE HEALTHCARE DATA..."))
  healthcare_full_imp <- rfImpute(x=as.data.frame(subset_healthcare), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  print(paste0(bootstrap_iteration, " - IMPUTE DEMOGRAPHIC DATA..."))
  demographic_full_imp <- rfImpute(x=as.data.frame(subset_demographic), y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  print(paste0(bootstrap_iteration, " - IMPUTE BY CATEGORY COMPLETE"))
  
  ##--------------------------------------------------------
  ##  Remove Unwanted Variables With Poor Interpretation  --
  ##--------------------------------------------------------
  # Columns Index to Keep by Category
  keep_index_covid <- which(colnames(covid_full_imp) %in% keep_cols_all)
  keep_index_disease <- which(colnames(disease_full_imp) %in% keep_cols_all)
  keep_index_econ <- which(colnames(econ_full_imp) %in% keep_cols_all)
  keep_index_environment <- which(colnames(environment_full_imp) %in% keep_cols_all)
  keep_index_food <- which(colnames(food_full_imp) %in% keep_cols_all)
  keep_index_healthcare <- which(colnames(healthcare_full_imp) %in% keep_cols_all)
  keep_index_demographic <- which(colnames(demographic_full_imp) %in% keep_cols_all)
  # Subset Categories by Index
  covid_subset_imp <- subset(covid_full_imp, select=keep_index_covid)
  disease_subset_imp <- subset(disease_full_imp, select=keep_index_disease)
  econ_subset_imp <- subset(econ_full_imp, select=keep_index_econ)
  environment_subset_imp <- subset(environment_full_imp, select=keep_index_environment)
  food_subset_imp <- subset(food_full_imp, select=keep_index_food)
  healthcare_subset_imp <- subset(healthcare_full_imp, select=keep_index_healthcare)
  demographic_subset_imp <- subset(demographic_full_imp, select=keep_index_demographic)
  
  ##---------------------------------
  ##  Model Selection by Category  --
  ##---------------------------------
  # Drop by VIF by Category
  expl_covid <- colnames(covid_subset_imp)[-1]
  expl_disease <- colnames(disease_subset_imp)[-1]
  expl_econ <- colnames(econ_subset_imp)[-1]
  expl_environment <- colnames(environment_subset_imp)[-1]
  expl_food <- colnames(food_subset_imp)[-1]
  expl_healthcare <- colnames(healthcare_subset_imp)[-1]
  expl_demographic <- colnames(demographic_subset_imp)[-1]
  
  after_drop_covid <- gvif_drop(resp, expl_covid, covid_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_disease <- gvif_drop(resp, expl_disease, disease_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_econ <- gvif_drop(resp, expl_econ, econ_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_environment <- gvif_drop(resp, expl_environment, environment_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_food <- gvif_drop(resp, expl_food, food_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_healthcare <- gvif_drop(resp, expl_healthcare, healthcare_subset_imp, vif_max=5, glmtype=2, maxit=100)
  after_drop_demographic <- gvif_drop(resp, expl_demographic, demographic_subset_imp, vif_max=5, glmtype=2, maxit=100)
  
  # Backwards Selection Using AICc by Category
  step_drop_vif_covid <- step2.glm(resp, after_drop_covid, covid_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_disease <- step2.glm(resp, after_drop_disease, disease_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_econ <- step2.glm(resp, after_drop_econ, econ_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_environment <- step2.glm(resp, after_drop_environment, environment_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_food <- step2.glm(resp, after_drop_food, food_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_healthcare <- step2.glm(resp, after_drop_healthcare, healthcare_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  step_drop_vif_demographic <- step2.glm(resp, after_drop_demographic, demographic_subset_imp, "AICc", Gamma(link="log"), maxit=100)
  print(paste0(bootstrap_iteration, " - MODEL SELECTION BY CATEGORY COMPLETE"))
  
  ##-------------------------------------------------------------------
  ##  Combine Significant Variables by Category (Imputed & Missing)  --
  ##-------------------------------------------------------------------
  # Significant Variables
  sig_covid <- all.vars(formula(step_drop_vif_covid)[-1])
  sig_disease <- all.vars(formula(step_drop_vif_disease)[-1])
  sig_econ <- all.vars(formula(step_drop_vif_econ)[-1])
  sig_environment <- all.vars(formula(step_drop_vif_environment)[-1])
  sig_food <- all.vars(formula(step_drop_vif_food)[-1])
  sig_healthcare <- all.vars(formula(step_drop_vif_healthcare)[-1])
  sig_demographic <- all.vars(formula(step_drop_vif_demographic)[-1])
  # Combine Significant Variables Imputed
  sig_covid_imputed <- subset(covid_full_imp, select=sig_covid)
  sig_disease_imputed <- subset(disease_full_imp, select=sig_disease)
  sig_econ_imputed <- subset(econ_full_imp, select=sig_econ)
  sig_environment_imputed <- subset(environment_full_imp, select=sig_environment)
  sig_food_imputed <- subset(food_full_imp, select=sig_food)
  sig_healthcare_imputed <- subset(healthcare_full_imp, select=sig_healthcare)
  sig_demographic_imputed <- subset(demographic_full_imp, select=sig_demographic)
  cat_sig_combined_imputed <- cbind(sig_covid_imputed, sig_disease_imputed, sig_econ_imputed,
                                    sig_environment_imputed, sig_food_imputed,
                                    sig_healthcare_imputed, sig_demographic_imputed)
  # Combine Significant Variables Missing
  sig_covid_miss <- subset(subset_covid, select=sig_covid)
  sig_disease_miss <- subset(subset_disease, select=sig_disease)
  sig_econ_miss <- subset(subset_econ, select=sig_econ)
  sig_environment_miss <- subset(subset_environment, select=sig_environment)
  sig_food_miss <- subset(subset_food, select=sig_food)
  sig_healthcare_miss <- subset(subset_healthcare, select=sig_healthcare)
  sig_demographic_miss <- subset(subset_demographic, select=sig_demographic)
  cat_sig_combined_miss <- cbind(sig_covid_miss, sig_disease_miss, sig_econ_miss,
                                 sig_environment_miss, sig_food_miss,
                                 sig_healthcare_miss, sig_demographic_miss)
  # Imputed Combined Significant Variables Missing Data
  print(paste0(bootstrap_iteration, " - IMPUTE CATEGORY MISS DATA..."))
  cat_sig_combined_miss_imputed_w_res <- rfImpute(x=as.data.frame(cat_sig_combined_miss),y=total_confirmed_deaths_due_to_covid_19_per_million_people)
  # Add Response Variable for Pre-Imputed Data
  cat_sig_combined_imputed_w_res <- cbind(bootstrap_response, cat_sig_combined_imputed)
  
  print(paste0(bootstrap_iteration, " - IMPUTE BY CATEGORY MISS COMPLETE"))
  
  ##-------------------------------------------------------------
  ##  Model Selection on Categories Combined (Imputed & Miss)  --
  ##-------------------------------------------------------------
  # For Imputed Model
  expl_imputed <- colnames(cat_sig_combined_imputed_w_res)[-1]
  after_drop_imputed <- gvif_drop(resp, expl_imputed, cat_sig_combined_imputed_w_res, vif_max=5, glmtype=2, maxit=100)
  step_drop_vif_imputed <- step2.glm(resp, after_drop_imputed, cat_sig_combined_imputed_w_res, "AICc", Gamma(link="log"), maxit=100)
  # For Miss Model
  expl_miss <- colnames(cat_sig_combined_miss_imputed_w_res)[-1]
  after_drop_miss <- gvif_drop(resp, expl_miss, cat_sig_combined_miss_imputed_w_res, vif_max=5, glmtype=2, maxit=100)
  step_drop_vif_miss <- step2.glm(resp, after_drop_miss, cat_sig_combined_miss_imputed_w_res, "AICc", Gamma(link="log"), maxit=100)
  print(paste0(bootstrap_iteration, " - MODEL SELECTION ON CATEGORIES COMBINED IMP & MIS COMPLETE"))
  
  ##------------------------------------
  ##  Add Included Variables to List  --
  ##------------------------------------
  randomForest_included_vars_imputed <- all.vars(formula(step_drop_vif_imputed)[-1])
  randomForest_included_vars_miss <- all.vars(formula(step_drop_vif_miss)[-1])
  randomForest_imp_included_vars_bootstrap <- c(randomForest_imp_included_vars_bootstrap, randomForest_included_vars_imputed)
  randomForest_mis_included_vars_bootstrap <- c(randomForest_mis_included_vars_bootstrap, randomForest_included_vars_miss)
  
  print(paste0("BOOTSTRAP ITERATION #", bootstrap_iteration, " COMPLETE"))
}
toc()

##----------------------------------------------------------------
##         Bootstrap Models Included Variable Frequency         --
##----------------------------------------------------------------
randomForest_imp_model_var_freq <- data.frame(table(randomForest_imp_included_vars_bootstrap))
randomForest_miss_model_var_freq <- data.frame(table(randomForest_mis_included_vars_bootstrap))
randomForest_full_model_var_freq <- data.frame(table(randomForest_ful_included_vars_bootstrap))

write.csv(randomForest_imp_model_var_freq, file="GLM (Random Forest (2nd))/Bootstrap/randomForest_cat_imp_var_freq.csv")
write.csv(randomForest_miss_model_var_freq, file="GLM (Random Forest (2nd))/Bootstrap/randomForest_cat_mis_var_freq.csv")
write.csv(randomForest_full_model_var_freq, file="GLM (Random Forest (2nd))/Bootstrap/randomForest_cat_ful_var_freq.csv")

View(randomForest_imp_model_var_freq)
View(randomForest_miss_model_var_freq)
View(randomForest_full_model_var_freq)