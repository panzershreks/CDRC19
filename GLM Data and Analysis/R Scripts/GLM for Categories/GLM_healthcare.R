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

healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")

# Remove first column 
healthcare_all_missing_clean <- subset(healthcare_all_mis, select = -c(1,2))

healthcare_all_missing_clean <- clean_names(healthcare_all_missing_clean)

# impute data
set.seed(100)
healthcare_all_rf <- missForest(as.matrix(healthcare_all_missing_clean))
healthcare_imputed_rf <- as.data.frame.matrix(healthcare_all_rf$ximp)

# define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(healthcare_imputed_rf)
expl <- expl[-1]

after_drop <- gvif_drop(resp, expl, healthcare_imputed_rf, vif_max=5, glmtype=2, maxit=100)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm2(final_formula, healthcare_imputed_rf, family = Gamma(link = "log"), maxit=100)
vif(final_model)

after_drop

step_final_model <- step2.glm(resp, after_drop, healthcare_imputed_rf, "AICc", Gamma(link="log"), maxit=500)
summary(step_final_model)

par(mfrow = c(2, 2))
plot(step_final_model)

all.vars(formula(step_final_model)[-1])

# Take variables from step function and create new dataframe (these have missing values )
healthcare_sigvars_missing_GLM <- subset(healthcare_all_missing_clean,
                                         select = c("total_confirmed_deaths_due_to_covid_19_per_million_people",
                                                    "all_causes_disability_adjusted_life_years_who_2015",
                                                    "share_of_population_covered_by_health_insurance_ilo_2014",
                                                    "beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd",
                                                    "for_profit_privately_owned_hospitals_per_million_population_oecd",
                                                    "nurses_per_1_000_population_oecd",
                                                    "publicly_owned_hospitals_per_million_population_oecd",
                                                    "surgical_specialists_per_1_000_population_oecd",
                                                    "hospital_beds_per_1_000_population_oecd",
                                                    "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure")) 

write.csv(healthcare_sigvars_missing_GLM,
          file = "GLM Data and Analysis/Significant Variables/Categories with Missing/healthcare_sigvars_missing_GLM.csv",
          row.names = TRUE)

# Subset after imputation now 

healthcare_sigvars_completed_GLM <- subset(healthcare_imputed_rf,
                                           select = c("total_confirmed_deaths_due_to_covid_19_per_million_people",
                                                      "all_causes_disability_adjusted_life_years_who_2015",
                                                      "share_of_population_covered_by_health_insurance_ilo_2014",
                                                      "beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd",
                                                      "for_profit_privately_owned_hospitals_per_million_population_oecd",
                                                      "nurses_per_1_000_population_oecd",
                                                      "publicly_owned_hospitals_per_million_population_oecd",
                                                      "surgical_specialists_per_1_000_population_oecd",
                                                      "hospital_beds_per_1_000_population_oecd",
                                                      "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure"))

write.csv(healthcare_sigvars_completed_GLM,
          file = "GLM Data and Analysis/Significant Variables/Categories Complete/healthcare_sigvars_completed_GLM.csv",
          row.names = TRUE)
