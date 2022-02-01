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
library('missForest')
library(readr)


# We read in the data that we required and we remove the countries which have no response variable value.

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -c(1,2))
clean_disease <- clean_disease[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                        145, 151, 156, 171,173,177,178, 186,193),]

# We explore the missing values across the dataset.

disease_missing_vis <- vis_miss(clean_disease, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# ggsave(disease_missing_vis, file="disease_missing_vis.png", height = 8, width = 8)


# We now impute our missing data using random forests.

set.seed(100)
disease_rf <- missForest(as.matrix(clean_disease))

# We save our imputed dataset as a dataframe
disease_data <- as.data.frame.matrix(disease_rf$ximp)

# write.csv(disease_data,"disease_data_complete.csv", row.names = TRUE)


# We will now look at the correlations between our variables

Mcor <- disease_data
dis_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

#dis_cor_matrix

# # We now will use a function we have written to carry out VIF to 
# find what variables we should use in our model.

resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"
expl <- c("age_standardised_diabetes_prevalence_male","age_standardised_diabetes_prevalence_female",
          "crude_diabetes_prevalence_male",
          "crude_diabetes_prevalence_female",
          "ratio_of_diabetes_to_obesity_prevalence",
          "ratio_of_diabetes_to_overweight_prevalence",
          "deaths_no_access_to_handwashing_facility_sex_both_age_70_number",
          "deaths_unsafe_sanitation_sex_both_age_70_number",
          "deaths_unsafe_water_source_sex_both_age_70_number",
          "deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate",
          "deaths_unsafe_sanitation_sex_both_age_70_rate",
          "deaths_unsafe_water_source_sex_both_age_70_rate",
          "male_lung_cancer_deaths_per_100_000_who_iarc_2016",
          "dementia_ihme_2017","cardiovascular_diseases_ihme_2017",
          "stroke_ihme_2017","kidney_disease_ihme_2017","respiratory_disease_ihme_2017",
          "liver_disease_ihme_2017","diabetes_blood_and_endocrine_disease_ihme_2017",
          "digestive_disease_ihme_2017","hepatitis_ihme_2017","meningitis_ihme_2017",
          "cancers_ihme_2017","parkinsons_disease_ihme_2017",
          "prevalence_of_obesity_both_sexes_who_2019",
          "prevalence_of_obesity_male_who_2019","prevalence_of_obesity_female_who_2019")

after_drop <- gvif_drop(resp, expl, disease_data)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula, disease_data)
vif(final_model)

step_disease <- step(final_model)
summary(step_disease)

# We now plot our model assumptions:

par(mfrow = c(2, 2))
plot(step_disease)












