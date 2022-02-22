library(readr)
library(corrplot)
library(psych)
# We first import the selected, imputed variables from each category.

combined_imputed_response <- read_csv("GLM Take 2/Combined Model/combined_imputed_response.csv")
combined_imputed_response <- clean_names(combined_imputed_response)
combined_imputed_response <- subset(combined_imputed_response, select = -1)

# We will now subset our data into each category before doing summary statistics on it.

response <- subset(combined_imputed_response, select = 1)
food_data <- subset(combined_imputed_response, select = c(2,7))
disease_data <- subset(combined_imputed_response, select = c(3:6))
econ_data <- subset(combined_imputed_response, select = c(8:13))
covid_data <- subset(combined_imputed_response, select = c(14:17))
health_data <- subset(combined_imputed_response, select = c(18:27))
demog_data <- subset(combined_imputed_response, select = c(28:30))
enviro_data <- subset(combined_imputed_response, select = c(31:32))

# Food Data

food_data
cor(food_data$healthy_diet_cost_percent_cannot_afford, food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019)

plot(food_data$healthy_diet_cost_percent_cannot_afford, response$total_confirmed_deaths_due_to_covid_19_per_million_people)
plot(food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019, response$total_confirmed_deaths_due_to_covid_19_per_million_people)

food_1 <- ggplot(data = data.frame(x =food_data$healthy_diet_cost_percent_cannot_afford, 
                         y = response$total_confirmed_deaths_due_to_covid_19_per_million_people), 
       aes(x=x, y=y)) + geom_point() + ggtitle("Diet and Covid-19 Deaths") + xlab("Healthy Diet Cost Percent Cannot Afford") + 
  ylab("Total Confirmed Deaths due to Covid-19 per Million People") + geom_smooth(method = "lm", se = FALSE)

food_2 <- ggplot(data = data.frame(x =food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019, 
                                   y = response$total_confirmed_deaths_due_to_covid_19_per_million_people), 
                 aes(x=x, y=y)) + geom_point() + ggtitle("Life Satisfaction and Covid-19 Deaths") + xlab("Life Satisfaction Indicator") + 
  ylab("Total Confirmed Deaths due to Covid-19 per Million People") + geom_smooth(method = "lm", se = FALSE)

ggsave(food_1, filename = "GLM Take 2//Summary Statistics//Report Graphs//food_1.png", height = 6, width = 14, units = "in")
ggsave(food_2, filename = "GLM Take 2//Summary Statistics//Report Graphs//food_2.png", height = 6, width = 14, units = "in")

# Disease Data

disease_data
summary(disease_data$prevalence_of_obesity_both_sexes_who_2019)
cor(disease_data$prevalence_of_obesity_both_sexes_who_2019, response$total_confirmed_deaths_due_to_covid_19_per_million_people)
disease_1 <- ggplot(data = data.frame(x = disease_data$prevalence_of_obesity_both_sexes_who_2019, 
                                   y = response$total_confirmed_deaths_due_to_covid_19_per_million_people), 
                 aes(x=x, y=y)) + geom_point() + ggtitle("Obesity and Covid-19 Deaths") + xlab("Obesity Prevelance in Both Sexes") + 
  ylab("Total Confirmed Deaths due to Covid-19 per Million People") + geom_smooth(method = "lm", se = FALSE)

ggsave(disease_1, filename = "GLM Take 2//Summary Statistics//Report Graphs//disease_1.png", height = 6, width = 14, units = "in")

cor(disease_data$age_standardised_diabetes_prevalence_male, disease_data$age_standardised_diabetes_prevalence_female)

disease_cor <- corPlot(disease_data, labels = c('Pop. Without Sanitation Access', 'Male Diabetes Prev.', 'Female Diabetes Prev.', 'Obesity Prev.'), 
                       main = "Disease Correlation", MAR = 9, diag = FALSE, cex = 2, xlas = 2)


png(file = "GLM Take 2//Summary Statistics//Report Graphs//disease_cor.png", width = 900, height = 900)
corPlot(disease_data, labels = c('Pop. Without Sanitation Access', 'Male Diabetes Prev.', 'Female Diabetes Prev.', 'Obesity Prev.'), 
        main = "Disease Correlation", MAR = 9, diag = FALSE, cex = 2, xlas = 2)
dev.off()


# Environmental Data

econ_data




