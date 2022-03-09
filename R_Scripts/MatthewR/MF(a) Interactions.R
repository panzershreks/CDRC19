# MF(a) interactions
library(interactions)
library(MuMIn)
library(ggplot2)
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

AICc(fit_a)
AICc(fit_b)

interact_plot(fit_b, pred = prevalence_of_obesity_both_sexes_who_2019, 
              modx = age_standardised_diabetes_prevalence_male, plot.points = TRUE, 
              main.title = "Interaction Between Obesity and Diabetes")


grid.lines=100
X3 <- combined_imputed_res$prevalence_of_obesity_both_sexes_who_2019
X5 <- combined_imputed_res$age_standardised_diabetes_prevalence_male
b3 <- 0.2440961
b5 <- 0.2599402
b6 <- -0.0144685

X3grid <- seq(min(X3), max(X3), length.out = grid.lines)
X5grid <- seq(min(X5), max(X5), length.out = grid.lines)
data.fit <- expand.grid(X3 = X3grid, X5 = X5grid)
data.fit['inter'] <- b3 * data.fit['X3'] + b5 * data.fit['X5'] + b6 * data.fit['X3'] * data.fit['X5']
ggplot(data.fit, aes(x=X3, y=X5, z=inter)) + geom_contour_filled(bins=20)


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

