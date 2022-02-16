library(readr)
library("janitor")
library(naniar)
library(ggplot2)
library('missForest')
library(car)

source("R_Scripts/EmanR/step2.R")
source("R_Scripts/EmanR/automate_vif.R")

combined_all_missing <- read_csv("GLM Data and Analysis/Combined CSV/combined_all_missing.csv")
combined_all_missing <- clean_names(combined_all_missing)
combined_all_missing <- subset(combined_all_missing, select = -1)

# now subset disease variables and response
# response = column 1
# disease = 19 -> 46
# food/water = 2 -> 18 + world stats = 47

food_missing <- subset(combined_all_missing, select = c(1:18, 47))

# impute
set.seed(100)
food_rf <- missForest(as.matrix(food_missing))
food_imputed <- as.data.frame.matrix(food_rf$ximp)

# define response and explanatory variables
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- colnames(food_imputed)
expl <- expl[-1]


after_drop <- gvif_drop(resp, expl, food_imputed, vif_max=5, glmtype=1, maxit=100) # glm and glm2 both converge
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- glm(final_formula, food_imputed, family = Gamma(link = "log"))
vif(final_model)

after_drop

step_AICc_mod <- step2.glm(resp, after_drop, food_imputed, "AICc", Gamma(link="log"), maxit=100)
summary(step_AICc_mod)

par(mfrow = c(2, 2))
plot(step_AICc_mod)

all.vars(formula(step_AICc_mod)[-1])

# We now want to take the variables output from the step function and put them into a dataframe.
# First we do this for the imputed data

food_imputed_sig_var <- subset(food_imputed, select=c(
  cost_of_calorie_sufficient_diet_2017_usd_per_day,
  cost_of_nutrient_adequate_diet_2017_usd_per_day,
  healthy_diet_cost_percent_of_1_20_poverty_line,
  nutrient_adequate_diet_cost_percent_of_average_food_expenditure,
  healthy_diet_cost_percent_cannot_afford,
  calorie_sufficient_diet_cost_number_cannot_afford,
  nutrient_adequate_diet_cost_number_cannot_afford,
  life_satisfaction_in_cantril_ladder_world_happiness_report_2019
))

write.csv(food_imputed_sig_var,"GLM Data and Analysis/Significant Variables/Categories Complete/food_imputed_sig_var.csv", row.names = TRUE)

# Now we do this for the data before random forests

food_missing_sig_var <- subset(food_missing, select=c(
  cost_of_calorie_sufficient_diet_2017_usd_per_day,
  cost_of_nutrient_adequate_diet_2017_usd_per_day,
  healthy_diet_cost_percent_of_1_20_poverty_line,
  nutrient_adequate_diet_cost_percent_of_average_food_expenditure,
  healthy_diet_cost_percent_cannot_afford,
  calorie_sufficient_diet_cost_number_cannot_afford,
  nutrient_adequate_diet_cost_number_cannot_afford,
  life_satisfaction_in_cantril_ladder_world_happiness_report_2019
))

write.csv(food_missing_sig_var,"GLM Data and Analysis/Significant Variables/Categories with Missing/food_missing_sig_var.csv", row.names = TRUE)






