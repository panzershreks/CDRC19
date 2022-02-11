library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(tidyverse)
library(readr)
library(dplyr)
library(naniar)
library(car)
library(visdat)
library(mice)
library(UpSetR)
library(janitor)
library(corrplot)
library(gt)
library(missForest)
library(stargazer)

glm_clean_covid <- read.csv("~/Documents/University/YEAR 4/ALL YEAR/MATHEMATICAL PROJECT/ANALYSIS/CDRC19/Combined DataFrame Work/CSV Files/Clean/clean_covid.csv")
glm_clean_covid <- clean_names(glm_clean_covid)
glm_clean_covid <- subset(glm_clean_covid, select = -c(1,2))
glm_clean_covid$income_support <- as.factor(glm_clean_covid$income_support)
glm_clean_covid$debt_relief <- as.factor(glm_clean_covid$debt_relief)
glm_clean_covid <- glm_clean_covid[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                              145, 151, 156, 171,173,177,178, 186,193), ]
covid_missing_vis <- vis_miss(glm_clean_covid, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
covid_missing_vis
drop_2 <- c("weekly_new_icu_admissions",
            "weekly_new_icu_admissions_per_million",
            "daily_icu_occupancy",
            "daily_icu_occupancy_per_million",
            "weekly_new_hospital_admissions",
            "weekly_new_hospital_admissions_per_million",
            "total_covid_19_tests_performed_per_million_people",
            "daily_hospital_occupancy",
            "daily_hospital_occupancy_per_million",
            "total_covid_19_tests_performed",
            "importance_larson_et_al_2016",	
            "safety_larson_et_al_2016",	
            "effectiveness_larson_et_al_2016",
            "alpha",
            "b_1_1_277",
            "b_1_1_302",
            "b_1_160",
            "b_1_177",
            "b_1_221",
            "b_1_258",
            "b_1_367",
            "beta",	
            "delta",	
            "epsilon",	
            "eta",		
            "gamma",	
            "iota",		
            "kappa",	
            "s_677h_robin1",	
            "s_677p_pelican",
            "others",		
            "non_who",
            "lambda",
            "b_1_621",
            "b_1_620",
            "b_1_1_519",
            "emergency_investment_healthcare",
            "investment_vaccines",
            "international_support",
            # take out last three as they are all 0 with a few NAs
            # all sources for stringency index so do not need
            "school_closures",
            "workplace_closures",
            "cancel_public_events",
            "close_public_transport",
            "public_information_campaigns",
            "restrictions_internal_movements",
            "international_travel_controls",
            "contact_tracing",
            "restriction_gatherings",
            "stay_home_requirements",
            "testing_policy",
            "facial_coverings",
            "vaccination_policy",
            "fiscal_measures",
            "population_with_access_to_improved_sanitation_x")

glm_clean_covid <- glm_clean_covid[,!(names(glm_clean_covid) %in% drop_2)]
#cov_missing_data <- rf_glm_clean_covid[,!(names(rf_glm_clean_covid) %in% drop_3)]
#cov_miss_vis <- vis_miss(cov_missing_data, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
#cov_miss_vis
#summary(cov_missing_data)
glm_clean_covid <- glm_clean_covid[,!(names(glm_clean_covid) %in% drop_3)]
glm_clean_covid
#write.csv(glm_clean_covid, file = "glm_clean_covid.csv", row.names = TRUE)
#covid_rf <- missForest(as.matrix(rf_glm_clean_covid))
#glm_glm_covid_data <- covid_rf$ximp
#glm_covid_data <- as.data.frame.matrix(glm_covid_data)

gvif_drop <- function(resp_var, expl_var, data, vif_max=7) {
  gvif_max <- vif_max ^ 0.5
  lm_formula <- lm_formula_paster(resp_var, expl_var)
  model <- glm(lm_formula, data, family = Gamma(link ="log"))
  vif_mod <- vif(model)
  try(gvif <- vif_mod, silent = TRUE)
  try(gvif <- vif_mod[,3], silent = TRUE)
  if (is.null(dim(vif_mod))) {
    gvif_max <- vif_max
  }
  while (max(gvif) > gvif_max) {
    expl_var <- expl_var[-(which.max(gvif))]
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- glm(lm_formula, data, family = Gamma(link ="log"))
    vif_mod <- vif(model)
    try(gvif <- vif_mod, silent = TRUE)
    try(gvif <- vif_mod[,3], silent = TRUE)
    if (is.null(dim(vif_mod))) {
      gvif_max <- vif_max
    }
  }
  return (expl_var)
}

#' create a lm formula from list of variables
#' helper function for gvif_drop
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @return str of formula using the variables provided
lm_formula_paster <- function(resp_var, expl_var) {
  form <- paste0(resp_var, "~")
  for (var in head(expl_var, -1)) {
    form <- paste0(form, var, "+")
  }
  form <- paste0(form, tail(expl_var, 1))
  return (form)
}
set.seed(100)
glm_covid_rf <- missForest(data.frame(glm_clean_covid))
glm_covid_data <- glm_covid_rf$ximp
drop_3 <- c("has_population_5m_and_had_100_cases_21_days_ago_and_has_testing_data",
            "total_confirmed_deaths_due_to_covid_19",
            "population_without_access_to_improved_sanitation_x",
            "daily_new_confirmed_cases_of_covid_19",
            "daily_new_confirmed_deaths_due_to_covid_19",
            "total_confirmed_cases_of_covid_19",
            "total_confirmed_deaths_due_to_covid_19",
            "daily_new_confirmed_cases_of_covid_19_per_million_people",
            "daily_new_confirmed_deaths_due_to_covid_19_per_million_people",
            "total_confirmed_cases_of_covid_19_per_million_people",
            "days_since_the_total_confirmed_cases_of_covid_19_reached_100",
            "days_since_the_total_confirmed_deaths_of_covid_19_reached_5",
            "days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1",
            "days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1",
            "case_fatality_rate_of_covid_19",	"case_fatality_rate_of_covid_19_only_observations_with_100_cases",
            "days_since_30_daily_new_confirmed_cases_recorded",
            "days_since_50_daily_new_confirmed_cases_recorded",
            "days_since_10_daily_new_confirmed_deaths_recorded",
            "days_since_5_daily_new_confirmed_deaths_recorded",
            "days_since_3_daily_new_confirmed_deaths_recorded",
            "daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned",
            "daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned",
            "days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1",
            "days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1",
            "daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned",
            "daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned",
            "daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned",
            "daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned",
            "days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30",
            "days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5",
            "days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01",
            "daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned",
            "daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned",
            "days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m",
            "doubling_days_of_total_confirmed_cases_3_day_period",
            "doubling_days_of_total_confirmed_cases_7_day_period",
            "doubling_days_of_total_confirmed_deaths_3_day_period",
            "doubling_days_of_total_confirmed_deaths_7_day_period",
            "weekly_cases",	"weekly_deaths",	"weekly_case_growth",
            "weekly_death_growth",	"biweekly_cases",	"biweekly_deaths",
            "biweekly_case_growth",	"biweekly_death_growth",
            "weekly_cases_per_million_people",	"weekly_deaths_per_million_people",
            "biweekly_cases_per_million_people",	"biweekly_deaths_per_million_people",
            "case_fatality_rate_of_covid_19_short_term")
glm_covid_data <- glm_covid_data[,!(names(glm_covid_data) %in% drop_3)]
glm_covid_data$income_support <- as.numeric(glm_covid_data$income_support)
glm_covid_data$debt_relief <- as.numeric(glm_covid_data$debt_relief)
#Mcorcov <- glm_covid_data
#covid_cor_matrix <- vis_cor(Mcorcov) + theme(axis.text.x = element_text(angle = 90)) + 
#  ggtitle("Correlation Matrix")
#covid_cor_matrix
#write.csv(glm_covid_data, file = "rf_glm_covid_data.csv", row.names = TRUE)

# take out highly correlated variables
# 4 taken out are weekly_cases, weekly_deaths, case_fatality_rate_of_covid_19 and days_since_the_total_confirmed_cases_of_covid_19_reached_100

resp_var_covid <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl_var_covid <- c("stringency_index", "income_support", "debt_relief", "containment_index")
after_drop_covid <- gvif_drop(resp_var_covid, expl_var_covid, glm_covid_data)
final_formula_covid <- lm_formula_paster(resp_var_covid, after_drop_covid)
final_model_covid <- glm(final_formula_covid, glm_covid_data, family = Gamma(link = "log"))
vif(final_model_covid)
#stargazer(vif(final_model_covid), type='latex', summary=FALSE)
step_final_model_covid <-step(final_model_covid)
summary(step_final_model_covid)
par(mfrow = c(2, 2))
plot(step_final_model_covid)


# only include variables with significance level 0.05 of more... these include:
# glm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#stringency_index + income_support + debt_relief, family = Gamma(link = "log"), 
#data = glm_covid_data)

glm_covid_imputed_data_variables <- subset(glm_covid_data, select = c("income_support", "stringency_index", "debt_relief"))
#write.csv(glm_covid_imputed_data_variables, file = "glm_covid_imputed_data_variables.csv", row.names = TRUE)

# now another dataset with the significant variables from the data before it was imputed

glm_covid_clean_data_variables <- subset(glm_clean_covid, select = c("income_support", "stringency_index", "debt_relief"))
#write.csv(glm_covid_clean_data_variables, file = "glm_covid_clean_data_variables.csv", row.names = TRUE)




