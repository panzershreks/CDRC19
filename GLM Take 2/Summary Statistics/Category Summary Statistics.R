library(readr)
library(corrplot)
library(psych)
library(ggplot2)
library(ggpubr)
library(janitor)
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
health_data <- subset(combined_imputed_response, select = c(18:26)) # total disbursements has been excluded column 27
demog_data <- subset(combined_imputed_response, select = c(28:30))
enviro_data <- subset(combined_imputed_response, select = c(31:32))

# Food Data

food_data
cor(food_data$healthy_diet_cost_percent_cannot_afford, food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019)

plot(food_data$healthy_diet_cost_percent_cannot_afford, response$total_confirmed_deaths_due_to_covid_19_per_million_people)
plot(food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019, response$total_confirmed_deaths_due_to_covid_19_per_million_people)




food_1 <- ggplot(food_data, aes(x=healthy_diet_cost_percent_cannot_afford)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(healthy_diet_cost_percent_cannot_afford)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Access to Healthy Diets around the World") + xlab("Healthy Diet Cost Percent Cannot Afford") + ylab("Count")

food_2 <- ggplot(food_data, aes(x=life_satisfaction_in_cantril_ladder_world_happiness_report_2019)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(life_satisfaction_in_cantril_ladder_world_happiness_report_2019)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Life Satisfaction") + xlab("Life Satisfaction Indicator") + ylab("Count")


food_plot <- ggarrange(food_1, food_2 , labels = c("A", "B"),
                         ncol = 2, nrow = 1)

#ggsave(food_plot, filename = "GLM Take 2//Summary Statistics//Report Graphs//food_plot.png", height = 6, width = 14, units = "in")

sd(food_data$healthy_diet_cost_percent_cannot_afford)
sd(food_data$life_satisfaction_in_cantril_ladder_world_happiness_report_2019)


# Disease Data

disease_data$prevalence_of_obesity_both_sexes_who_2019
summary(disease_data)
summary(disease_data$prevalence_of_obesity_both_sexes_who_2019)
cor(disease_data$prevalence_of_obesity_both_sexes_who_2019, response$total_confirmed_deaths_due_to_covid_19_per_million_people)
disease_1 <- ggplot(data = data.frame(x = disease_data$prevalence_of_obesity_both_sexes_who_2019, 
                                   y = response$total_confirmed_deaths_due_to_covid_19_per_million_people), 
                 aes(x=x, y=y)) + geom_point() + ggtitle("Obesity and Covid-19 Deaths") + xlab("Obesity Prevelance in Both Sexes") + 
  ylab("Total Confirmed Deaths due to Covid-19 per Million People") + geom_smooth(method = "lm", se = FALSE)

#ggsave(disease_1, filename = "GLM Take 2//Summary Statistics//Report Graphs//disease_1.png", height = 4, width = 14, units = "in")

cor(disease_data$age_standardised_diabetes_prevalence_male, disease_data$age_standardised_diabetes_prevalence_female)

disease_cor <- corPlot(disease_data, labels = c('Pop. Without Sanitation Access', 'Male Diabetes Prev.', 'Female Diabetes Prev.', 'Obesity Prev.'), 
                       main = "Disease Correlation", MAR = 9, diag = FALSE, cex = 2, xlas = 2)


#png(file = "GLM Take 2//Summary Statistics//Report Graphs//disease_cor.png", width = 900, height = 900)
#corPlot(disease_data, labels = c('Pop. Without Sanitation Access', 'Male Diabetes Prev.', 'Female Diabetes Prev.', 'Obesity Prev.'), 
       # main = "Disease Correlation", MAR = 9, diag = FALSE, cex = 2, xlas = 2)
#dev.off()

dis_Cor <- cor(disease_data)
colnames(dis_Cor) <- c('Pop. w/out Sanitation Acc.', ' Males Diabetes Prev.', 'Female Diabetes Prev', 'Obesity Prev.')
rownames(dis_Cor) <- c('Pop. w/out Sanitation Acc.', ' Males Diabetes Prev.', 'Female Diabetes Prev', 'Obesity Prev.')


disease_cor <- corrplot(dis_Cor,addCoef.col = 'black', col = COL2('PiYG'), title = "Disease Correlation", mar = c(0,0,3,0), diag = FALSE)
file_path = "GLM Take 2//Summary Statistics//Report Graphs//disease_cor.png"
png(height = 1000, width = 1000,file = file_path)
corrplot(dis_Cor,addCoef.col = 'black', col = COL2('PiYG'), title = "Disease Correlation", mar = c(0,0,3,0),diag = FALSE)
dev.off()




# Environmental Data

enviro_1 <- ggplot(enviro_data, aes(x=death_rates_from_all_air_pollution_per_100_000)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(death_rates_from_all_air_pollution_per_100_000)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Distribution of Death Rates from All Air Polution per 100,000") + xlab("Death Rates from All Air Polution per 100,000") + ylab("Count")

enviro_2 <- ggplot(enviro_data, aes(x=yll_rates_from_all_air_pollution_per_100_000)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(yll_rates_from_all_air_pollution_per_100_000)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Distribution of Years of Life Lost from All Air Polution per 100,000") + xlab("YLL from All Air Polution Sources per 100,000") + ylab("Count")

enviro_plot <- ggarrange(enviro_1, enviro_2 , labels = c("A", "B"),
                        ncol = 2, nrow = 1)

#ggsave(enviro_plot, filename = "GLM Take 2//Summary Statistics//Report Graphs//enviro_plot.png", height = 6, width = 14, units = "in")

summary(enviro_data)

# Covid-19 Data

covid_data
covid_data$income_support <- as.factor(covid_data$income_support)
covid_data$debt_relief <- as.factor(covid_data$debt_relief)

covid_1 <- ggplot(covid_data, aes(x=income_support)) + geom_histogram(stat = 'count', fill="aquamarine3", color="black") +
  ggtitle("Income Support During Pandemic") + xlab("Income Support") + ylab("Count")

covid_2 <- ggplot(covid_data, aes(x=debt_relief)) + geom_histogram(stat = 'count', fill="aquamarine3", color="black") +
  ggtitle("Debt Relief During Pandemic") + xlab("Debt Relief") + ylab("Count")

covid_plot <- ggarrange(covid_1, covid_2 , labels = c("A", "B"),
                         ncol = 2, nrow = 1)

ggsave(covid_plot, filename = "GLM Take 2//Summary Statistics//Report Graphs//covid_plot.png", height = 6, width = 14, units = "in")

summary(covid_data$stringency_index)
cor(covid_data$stringency_index, covid_data$containment_index)

# Demographic Data

summary(demog_data)
dem_Cor <- cor(demog_data)
colnames(dem_Cor) <- c('Child Mortality Rate', 'Life Expec.', 'Total Pop.')
rownames(dem_Cor) <- c('Child Mortality Rate', 'Life Expec.', 'Total Pop.')

file_path = "GLM Take 2//Summary Statistics//Report Graphs//dem_Cor.png"
png(height = 1000, width = 1000,file = file_path)
corrplot(dem_Cor,addCoef.col = 'black', col = COL2('PiYG'), title = "Demographic Correlation", mar = c(0,0,3,0), diag = FALSE)
dev.off()


# Econ Variables

econ_data
econ_data$income_classification_world_bank_2017 <- as.factor(econ_data$income_classification_world_bank_2017)

summary(econ_data)

econ_1 <- ggplot(econ_data, aes(x=income_classification_world_bank_2017)) + geom_histogram(stat = 'count', fill="aquamarine3", color="black") +
  ggtitle("Income Classification") + xlab("Income") + ylab("Count")

#ggsave(econ_1, filename = "GLM Take 2//Summary Statistics//Report Graphs//econ_1.png", height = 6, width = 14, units = "in")

numeric_econ_data <- subset(econ_data, select = -2)
econ_Cor <- cor(numeric_econ_data)
colnames(econ_Cor) <- c('Mean Monthly Income', 'Gini Index', 'GDP/Capita','Nat. Pov. Lines', 'Mean Monthly/Capita Exp.')
rownames(econ_Cor) <- c('Mean Monthly Income', 'Gini Index', 'GDP/Capita','Nat. Pov. Lines', 'Mean Monthly/Capita Exp.')

file_path = "GLM Take 2//Summary Statistics//Report Graphs//econ_Cor.png"
png(height = 1000, width = 1000,file = file_path)
corrplot(econ_Cor,addCoef.col = 'black', col = COL2('PiYG'), title = "Economic Data Correlation", mar = c(0,0,3,0), diag = FALSE)
dev.off()

# Health Data


health_data

health_cor <- cor(health_data)
colnames(health_cor) <- c('All Causes Disab. Adj. Life Years','Share of Pop. covered by Health Ins.', 
                          'Health Exp./Capita', 'HAQ Index', 'Hosp./million', 'Nurses/1000','Physicians/1000', 'Hosp. beds/1000',
                          'Out of pocket exp./capita on healthcare')
rownames(health_cor) <- c('All Causes Disab. Adj. Life Years','Share of Pop. covered by Health Ins.', 
                          'Health Exp./Capita', 'HAQ Index', 'Hosp./million', 'Nurses/1000','Physicians/1000', 'Hosp. beds/1000',
                          'Out of pocket exp./capita on healthcare')

file_path = "GLM Take 2//Summary Statistics//Report Graphs//health_cor.png"
png(height = 1000, width = 1000,file = file_path)
corrplot(health_cor,addCoef.col = 'black', col = COL2('PiYG'), title = "Health Data Correlation", mar = c(0,0,3,0), diag = FALSE)
dev.off()

health_1 <- ggplot(health_data, aes(x=haq_index_ihme_2017)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(haq_index_ihme_2017)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Healthcare Access and Quality Index") + xlab("HAQ Index") + ylab("Count")

health_2 <- ggplot(health_data, aes(x=hospital_beds_per_1_000_population_oecd)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(hospital_beds_per_1_000_population_oecd)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Available Hospital Beds") + xlab("Hospital Beds per 1000 population") + ylab("Count")

health_plot <- ggarrange(health_1, health_2 , labels = c("A", "B"),
                       ncol = 2, nrow = 1)

ggsave(health_plot, filename = "GLM Take 2//Summary Statistics//Report Graphs//health_plot.png", height = 6, width = 14, units = "in")




