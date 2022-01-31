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

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -c(1,2))


set.seed(100)
disease_rf <- missForest(as.matrix(clean_disease))
disease_data <- disease_rf$ximp
disease_data <- as.data.frame.matrix(disease_data)

# write.csv(disease_data,"disease_rf.csv", row.names = TRUE)

full_model_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate +
                     male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_data)

vif(full_model_1)

# Remove prevalence_of_obesity_both_sexes_who_2019

d_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate +
                     male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_data)

vif(d_1)

# Remove deaths_unsafe_sanitation_sex_both_age_70_rate

d_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male + 
            age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
            deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
            deaths_unsafe_water_source_sex_both_age_70_rate +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_data)

vif(d_2)

# Remove deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate

d_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male + 
            age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
            deaths_unsafe_water_source_sex_both_age_70_rate +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_data)

vif(d_3)

# Remove age_standardised_diabetes_prevalence_female

d_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
            crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
            deaths_unsafe_water_source_sex_both_age_70_rate +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_data)

vif(d_4)

# Remove prevalence_of_obesity_female_who_2019

d_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
            crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
            deaths_unsafe_water_source_sex_both_age_70_rate +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_5)

# Remove age_standardised_diabetes_prevalence_male 

d_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
            crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
            deaths_unsafe_water_source_sex_both_age_70_rate +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_6)

# Remove deaths_unsafe_water_source_sex_both_age_70_rate 

d_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
            crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_7)

# Remove stroke_ihme_2017

d_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
            crude_diabetes_prevalence_male + 
            ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_8)

# Remove ratio_of_diabetes_to_obesity_prevalence 

d_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
            crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_9)

# Remove digestive_disease_ihme_2017

d_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
            crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
            deaths_no_access_to_handwashing_facility_sex_both_age_70_number +
            male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
            cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
            respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
            diabetes_blood_and_endocrine_disease_ihme_2017 + 
            hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
            parkinsons_disease_ihme_2017 + 
            prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_10)

# Remove meningitis_ihme_2017

d_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~
             crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number +
             male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_data)

vif(d_11)

step_disease <- step(d_11)
summary(step_disease)




