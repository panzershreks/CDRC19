# Combining the Significant Variables with Missing Data Together

library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)


# Matthew's Data

matthew_sig_var_w_missing <- read_csv("Final Model Data and Analysis/Categories with Missing/matthew_sig_var_w_missing.csv")
matthew_sig_var_w_missing <- clean_names(matthew_sig_var_w_missing)
matthew_sig_var_w_missing <- subset(matthew_sig_var_w_missing, select = -1)

# Emma's Data

healthcare_sigvars_missing <- read_csv("Final Model Data and Analysis/Categories with Missing/healthcare_sigvars_missing.csv")
healthcare_sigvars_missing <- clean_names(healthcare_sigvars_missing)
healthcare_sigvars_missing <- subset(healthcare_sigvars_missing, select = -1)

# Andy's Demographic Data - update this with the new data...

Not_Completed_data_demorgraphic_ <- read_csv("Final Model Data and Analysis/Categories with Missing/Not Completed data(demorgraphic).csv")
Not_Completed_data_demorgraphic_ <- clean_names(Not_Completed_data_demorgraphic_)
Not_Completed_data_demorgraphic_ <- subset(Not_Completed_data_demorgraphic_, select = -1)

# Eman's Economic Data

econ_significant_pre_random_forest <- read_csv("Final Model Data and Analysis/Categories with Missing/econ_significant_pre_random_forest.csv")
econ_significant_pre_random_forest <- clean_names(econ_significant_pre_random_forest)
econ_significant_pre_random_forest <- subset(econ_significant_pre_random_forest, select = -1)

# Bee Covid Data

rf_covid_clean_data_variables <- read_csv("Final Model Data and Analysis/Categories with Missing/rf_covid_clean_data_variables.csv")
rf_covid_clean_data_variables <- clean_names(rf_covid_clean_data_variables)
rf_covid_clean_data_variables <- subset(rf_covid_clean_data_variables, select = -1)

# Bee Environment Data

rf_enviroment_clean_data_variables <- read_csv("Final Model Data and Analysis/Categories with Missing/rf_enviroment_clean_data_variables.csv")
rf_enviroment_clean_data_variables <- clean_names(rf_enviroment_clean_data_variables)
rf_enviroment_clean_data_variables <- subset(rf_enviroment_clean_data_variables, select = -1)


# We now want to combine these dataframes into one big dataframe.

all_cat_with_missing <- cbind(matthew_sig_var_w_missing, econ_significant_pre_random_forest, healthcare_sigvars_missing, Not_Completed_data_demorgraphic_, rf_covid_clean_data_variables, rf_enviroment_clean_data_variables)

# write.csv(all_cat_with_missing,"all_cat_with_missing.csv", row.names = TRUE)


# We will now inspect the missing data in the combined significant variables.
vis_miss(all_cat_with_missing, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# We see that 18.1% of our data missing...


# We will now impute these data using random forest imputation method

set.seed(100)
all_cat_rf <- missForest(as.matrix(all_cat_with_missing))
all_cat_imputed <- as.data.frame.matrix(all_cat_rf$ximp)

# We see that we have no missing data now.
vis_miss(all_cat_imputed, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))



# We will now do model selection, and we begin by using VIF

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("age_standardised_diabetes_prevalence_male",
          "cardiovascular_diseases_ihme_2017",
          "meningitis_ihme_2017","prevalence_of_obesity_female_who_2019",
          "healthy_diet_cost_percent_cannot_afford",
          "income_classification_world_bank_2017","gdp_growth_from_previous_year_2020_q2",
          "gdp","national_poverty_lines_jolliffe_and_prydz_2016",
          "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
          "multidimensional_poverty_headcount_ratio_alkire_and_robles_2016",
          "share_of_people_who_disagree_vaccines_are_important_for_children_to_have",
          "share_of_people_who_disagree_vaccines_are_safe",
          "share_of_people_who_agree_vaccines_are_effective",
          "general_hospitals_per_million_population_oecd",
          "not_for_profit_privately_owned_hospitals_per_million_population_oecd",
          "psychiatric_care_beds_per_1_000_population_oecd",
          "publicly_owned_hospitals_per_million_population_oecd",
          "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure",
          "infant_mortality_rate","case_fatality_rate_of_covid_19_short_term",
          "biweekly_deaths_per_million_people","weekly_cases_per_million_people",
          "weekly_case_growth","doubling_days_of_total_confirmed_deaths_3_day_period",
          "days_since_10_daily_new_confirmed_deaths_recorded",
          "case_fatality_rate_of_covid_19_only_observations_with_100_cases",
          "days_since_30_daily_new_confirmed_cases_recorded","total_confirmed_deaths_due_to_covid_19",
          "daily_new_confirmed_cases_of_covid_19_per_million_people",
          "daily_new_confirmed_deaths_due_to_covid_19_per_million_people",
          "total_confirmed_cases_of_covid_19_per_million_people",
          "population_with_access_to_improved_sanitation_x",
          "debt_relief","stringency_index",
          "deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths",
          "yll_rates_from_anthropogenic_air_pollution_per_100_000")

after_drop <- gvif_drop(resp, expl, all_cat_imputed)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, all_cat_imputed)
vif(final_model)

step_all_cat_imputed <- step(final_model)
summary(step_all_cat_imputed)






