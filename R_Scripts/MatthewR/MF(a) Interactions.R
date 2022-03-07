# MF(a) interactions
library(interactions)
source("GLM Take 2/Combined Model/Full Model Imputation.R")

fit_a <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
              income_support + 
              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure +
              prevalence_of_obesity_both_sexes_who_2019 + age_standardised_diabetes_prevalence_male, 
             data = combined_imputed_res,family = Gamma(link = "log"))


fit_b <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
              income_support + 
              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure +
              prevalence_of_obesity_both_sexes_who_2019 + age_standardised_diabetes_prevalence_male + 
              prevalence_of_obesity_both_sexes_who_2019:age_standardised_diabetes_prevalence_male, 
             data = combined_imputed_res, family = Gamma(link = "log"))

AIC(fit_a)
AIC(fit_b)

interact_plot(fit_b, pred = prevalence_of_obesity_both_sexes_who_2019, 
              modx = age_standardised_diabetes_prevalence_male, plot.points = TRUE, 
              main.title = "Interaction Between Obesity and Diabetes")














#print(xtable(fit_b, type = "latex"), file = "All Model Latex/MF_a_interact.tex")

par(mfrow = c(2,2))
plot(fit_b, main="Automated Model GLM, AICc")

par(mfrow=c(1,1))
plot(fitted(fit_b), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  title("Fitted Values and Covid-19 Death Rates")


source("LM Data and Analysis/list_of_new_countries.R")
# code to add country text to the plot,
par(mfrow=c(1,1))
plot(fitted(fit_b), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  text(fitted(fit_b), combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people, 
       row.names(list_of_countries), cex=0.6, pos=4, col="red") + abline(a=0, b=1)

ratio_plot <- fitted(fit_b)/combined_imputed_res$total_confirmed_deaths_due_to_covid_19_per_million_people
plot(ratio_plot) + text(ratio_plot, row.names(list_of_countries), cex=0.6, pos=4, col="red") + 
  title("Ratio of Predicted and Actual Deaths")


  
  
  
  

