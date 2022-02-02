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
library(tictoc)
library(miceRanger)
library(olsrr)
library(missForest)

# Import data and basic pre-processing
# convert entities to factors and all other columns to numeric, coerce to NAs
econ_data_full <- read_csv("./../../Combined DataFrame Work/CSV Files/Clean/clean_economic.csv")
econ_data_full <- clean_names(econ_data_full)
entity_col <- subset(econ_data_full, select = entity)
econ_data_full <- subset(econ_data_full, select = -c(x1, entity))
econ_data_full[] <- lapply(econ_data_full, function(x) as.numeric(as.character(x)))
econ_data_full$entity <- entity_col$entity
econ_data_full$entity <- as.factor(econ_data_full$entity)
econ_data_full <- econ_data_full %>% relocate(entity)


# Percentage of missing values
missing_vars_tbl <- miss_var_summary(econ_data_full, sort_miss = TRUE)
missing_vars_tbl

drop_pmm <- c('population_y', 'pop', 'd4avgincome', 'd5avgincome', 'd6avgincome', 'd7avgincome')
econ_pmm <- econ_data_full[,!(names(econ_data_full) %in% drop_pmm)]
econ_pmm$income_classification_world_bank_2017 <- as.factor(econ_pmm$income_classification_world_bank_2017)
pmm_imp <- mice(data=econ_pmm, m=5, method='pmm', maxit=5)

foo_econ <- subset(econ_data_full, select=-c(entity))
bar_econ <- subset(foo_econ, select=-c(income_classification_world_bank_2017))
foo_impute <- mice(data = bar_econ, m = 5, method = c("pmm"), maxit = 5)

# Imputation
set.seed(100)
tic("mice imputation")
econ_full_impute <- mice(data = econ_data_full, m = 5, method = c("cart"), maxit = 5)
toc() # mice imputation: 4018.386 sec elapsed (~67 minutes)

#save(econ_full_impute, file = "econ_full_impute.RData") # save imputation mids object
load("econ_full_impute.RData") # load imputation mids object

#densityplot(econ_full_impute, xlim = c(2.5, 17.5), ylim = c(0, 0.4))
com_econ <- complete(econ_full_impute)
econ1 <- complete(econ_full_impute, 1)

View(miss_var_summary(econ1, sort_miss = TRUE))
summary(com_econ)


# Use econ1
# remove NA columns (failed to impute due to collinearity)
econ <- econ1[ , apply(econ1, 2, function(x) !any(is.na(x)))]

num_econ <- subset(econ, select = -1)
vis_cor(num_econ) + theme(axis.text.x = element_text(angle = 90))
cor_data_frame <- round(cor(num_econ),2)

vis_cor(subset(econ_data_full, select = -1))  + theme(axis.text.x = element_text(angle = 90))

# results are quite poor, tons of collinearity, doesn't make sense to drop e.g.
# d1avgincome but leave in d2avgincome...
# high correlation (>0.9) with mean, just use that instead.

to_drop <- c('pop', 'd1avgincome', 'd2avgincome', 'd3avgincome', 'd4avgincome',
             'd5avgincome', 'd6avgincome', 'd7avgincome', 'd8avgincome',
             'd9avgincome', 'd10avgincome', 'q1avgincome', 'q2avgincome',
             'q3avgincome', 'q4avgincome', 'q5avgincome', 'flag')

econ_data_drop <- econ_data_full[,!(names(econ_data_full) %in% to_drop)]
econ_data_drop$income_classification_world_bank_2017 <- as.factor(econ_data_drop$income_classification_world_bank_2017)
miss_var_summary(econ_data_drop, sort_miss = TRUE)

num_econ_data_drop <- subset(econ_data_drop, select=-c(entity, income_classification_world_bank_2017))
vis_cor(num_econ_data_drop) + theme(axis.text.x = element_text(angle = 90))


tic("mice imputation, econ_data_drop, pmm")
econ_drop_imp_pmm <- mice(data = econ_data_drop, m = 5, method = c("pmm"), maxit = 100)
toc() # mice imputation, econ_data_drop, pmm: x sec elapsed

tic("mice imputation, econ_data_drop, cart")
econ_drop_imp_cart <- mice(data = econ_data_drop, m = 5, method = c("cart"), maxit = 100)
toc() # mice imputation, econ_data_drop, cart: 1975.288 sec elapsed

tic("mice imputation, econ_data_drop, rf")
econ_drop_imp_rf <- mice(data = econ_data_drop, m = 5, method = c("rf"), maxit = 100)
toc() # mice imputation, econ_data_drop, rf: 1618.234 sec elapsed

save(econ_drop_imp_cart, file = "econ_drop_imp_cart.RData") # save imputation mids object
load("econ_drop_imp_cart.RData") # load imputation mids object
save(econ_drop_imp_rf, file = "econ_drop_imp_rf.RData") # save imputation mids object
load("econ_drop_imp_rf.RData") # load imputation mids object

econ_drop_1 <- complete(econ_drop_imp_cart, 1)
econ_drop_2 <- complete(econ_drop_imp_cart, 2)
econ_drop_3 <- complete(econ_drop_imp_cart, 3)
econ_drop_4 <- complete(econ_drop_imp_cart, 4)
econ_drop_5 <- complete(econ_drop_imp_cart, 5)

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("mean", "income_classification_world_bank_2017", "gini_index",
          "gdp_growth_from_previous_year_2020_q2", "gdp_per_capita_ppp_2011_wdi_2016",
          "pop2021", "gini_coefficient_world_bank_2016", "gdp_per_capita", "gdp",
          "cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017",
          "national_poverty_lines_jolliffe_and_prydz_2016",
          "multidimensional_poverty_headcount_ratio_alkire_and_robles_2016", 
          "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016",
          "mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "poverty_rate_50_percent_of_median_lis_key_figures_2018")

expl1 <- gvif_drop(resp, expl, econ_drop_1)
expl2 <- gvif_drop(resp, expl, econ_drop_2)
expl3 <- gvif_drop(resp, expl, econ_drop_3)
expl4 <- gvif_drop(resp, expl, econ_drop_4)
expl5 <- gvif_drop(resp, expl, econ_drop_5)

dropped_formula_1 <- lm_formula_paster(resp, expl1)
dropped_formula_2 <- lm_formula_paster(resp, expl2)
dropped_formula_3 <- lm_formula_paster(resp, expl3)
dropped_formula_4 <- lm_formula_paster(resp, expl4)
dropped_formula_5 <- lm_formula_paster(resp, expl5)

dropped_model_1 <- lm(dropped_formula_1, econ_drop_1)
dropped_model_2 <- lm(dropped_formula_2, econ_drop_2)
dropped_model_3 <- lm(dropped_formula_3, econ_drop_3)
dropped_model_4 <- lm(dropped_formula_4, econ_drop_4)
dropped_model_5 <- lm(dropped_formula_5, econ_drop_5)

stepAIC1 <- step(dropped_model_1, direction="backward")
stepAIC_names1 <- names(stepAIC1$coefficients)[-1]
stepBIC1 <- step(dropped_model_1, direction="backward", k=log(nrow(econ_drop_1)))
stepBIC_names1 <- names(stepBIC1$coefficients)[-1]

stepAIC2 <- step(dropped_model_2, direction="backward")
stepAIC_names2 <- names(stepAIC2$coefficients)[-1]
stepBIC2 <- step(dropped_model_2, direction="backward", k=log(nrow(econ_drop_2)))
stepBIC_names2 <- names(stepBIC2$coefficients)[-1]

stepAIC3 <- step(dropped_model_3, direction="backward")
stepAIC_names3 <- names(stepAIC3$coefficients)[-1]
stepBIC3 <- step(dropped_model_3, direction="backward", k=log(nrow(econ_drop_3)))
stepBIC_names3 <- names(stepBIC3$coefficients)[-1]

stepAIC4 <- step(dropped_model_4, direction="backward")
stepAIC_names4 <- names(stepAIC4$coefficients)[-1]
stepBIC4 <- step(dropped_model_4, direction="backward", k=log(nrow(econ_drop_4)))
stepBIC_names4 <- names(stepBIC4$coefficients)[-1]

stepAIC5 <- step(dropped_model_5, direction="backward")
stepAIC_names5 <- names(stepAIC5$coefficients)[-1]
stepBIC5 <- step(dropped_model_5, direction="backward", k=log(nrow(econ_drop_5)))
stepBIC_names5 <- names(stepBIC5$coefficients)[-1]

na.pad <- function(x,len){
  x[1:len]
}
makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(lapply(l,na.pad,len=maxlen),...)
}

step_var_names <- list(aic1=stepAIC_names1, bic1=stepBIC_names1,
                       aic2=stepAIC_names2, bic2=stepBIC_names2,
                       aic3=stepAIC_names3, bic3=stepBIC_names3,
                       aic4=stepAIC_names4, bic4=stepBIC_names4,
                       aic5=stepAIC_names5, bic5=stepBIC_names5)

compar <- makePaddedDataFrame(step_var_names)

full_mod <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
                 mean + income_classification_world_bank_2017 + gini_index +
                 gdp_growth_from_previous_year_2020_q2 + gdp_per_capita_ppp_2011_wdi_2016 +
                 pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
                 cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
                 national_poverty_lines_jolliffe_and_prydz_2016 +
                 multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                 percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
                 percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
                 percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
                 mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
                 median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
                 poverty_rate_50_percent_of_median_lis_key_figures_2018,
                 data = econ_drop_1)

ols_vif_tol(full_mod)

drop1_mod <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
               mean + income_classification_world_bank_2017 + gini_index +
               gdp_growth_from_previous_year_2020_q2 + gdp_per_capita_ppp_2011_wdi_2016 +
               pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
               cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
               national_poverty_lines_jolliffe_and_prydz_2016 +
               multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
               percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
               percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
               percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
               mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
               poverty_rate_50_percent_of_median_lis_key_figures_2018,
               data = econ_drop_1)

ols_vif_tol(drop1_mod)

drop2_mod <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
                  mean + income_classification_world_bank_2017 + gini_index +
                  gdp_growth_from_previous_year_2020_q2 + 
                  pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
                  cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
                  national_poverty_lines_jolliffe_and_prydz_2016 +
                  multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                  percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
                  mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
                  poverty_rate_50_percent_of_median_lis_key_figures_2018,
                data = econ_drop_1)

ols_vif_tol(drop2_mod)

drop3_mod <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
                  mean + income_classification_world_bank_2017 + gini_index +
                  gdp_growth_from_previous_year_2020_q2 + 
                  pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
                  cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
                  national_poverty_lines_jolliffe_and_prydz_2016 +
                  multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                  percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
                  poverty_rate_50_percent_of_median_lis_key_figures_2018,
                data = econ_drop_1)

ols_vif_tol(drop3_mod)

drop4_mod <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
                  income_classification_world_bank_2017 + gini_index +
                  gdp_growth_from_previous_year_2020_q2 + 
                  pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
                  cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
                  national_poverty_lines_jolliffe_and_prydz_2016 +
                  multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                  percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
                  percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
                  poverty_rate_50_percent_of_median_lis_key_figures_2018,
                data = econ_drop_1)

ols_vif_tol(drop4_mod)

fit <- with(econ_drop_imp_cart, drop4_mod)
summary(pool(fit))

step(drop4_mod, direction="backward", k=log(nrow(econ_drop_1)))

step(drop4_mod, direction="backward")

bic_mod <- lm(formula = total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                gdp_growth_from_previous_year_2020_q2 + national_poverty_lines_jolliffe_and_prydz_2016 + 
                multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                poverty_rate_50_percent_of_median_lis_key_figures_2018, 
              data = econ_drop_1)
summary(bic_mod)

fit_bic <- with(econ_drop_imp_cart, bic_mod)
summary(pool(fit_bic))

fit <- with(econ_drop_imp_cart, lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
                                   mean + income_classification_world_bank_2017 + gini_index +
                                   gdp_growth_from_previous_year_2020_q2 + gdp_per_capita_ppp_2011_wdi_2016 +
                                   pop2021 + gini_coefficient_world_bank_2016 + gdp_per_capita + gdp +
                                   cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017 +
                                   national_poverty_lines_jolliffe_and_prydz_2016 +
                                   multidimensional_poverty_headcount_ratio_alkire_and_robles_2016 + 
                                   percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016 +
                                   percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016 +
                                   percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016 +
                                   mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
                                   median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017 +
                                   poverty_rate_50_percent_of_median_lis_key_figures_2018))

summary(pool(fit))

#imp <- mice(nhanes, maxit = 2, m = 2)
#fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
#summary(pool(fit))


test_vif <- ols_vif_tol(full_mod)
test_vif$Variables
full_mod$terms
foo <- vif(full_mod)
foo$GVIF
foo[,0]
bar <- as.data.frame(foo)
row.names(bar)[1]



resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("mean", "income_classification_world_bank_2017", "gini_index",
          "gdp_growth_from_previous_year_2020_q2", "gdp_per_capita_ppp_2011_wdi_2016",
          "pop2021", "gini_coefficient_world_bank_2016", "gdp_per_capita", "gdp",
          "cost_of_closing_the_poverty_gap_in_int_2011_povcal_net_world_bank_2017",
          "national_poverty_lines_jolliffe_and_prydz_2016",
          "multidimensional_poverty_headcount_ratio_alkire_and_robles_2016", 
          "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016",
          "percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016",
          "mean_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
          "poverty_rate_50_percent_of_median_lis_key_figures_2018")
gvif_drop(resp, expl, econ_drop_1)
