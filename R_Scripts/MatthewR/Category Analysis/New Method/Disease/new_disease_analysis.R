# We install the required packages

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

# We import and clean the required data.

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -1)

disease_miss_vis <- vis_miss(clean_disease, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
# ggsave(disease_miss_vis, file="disease_miss_vis.png", height = 8, width = 8)


# We take a look at our correlations, by first ommiting the missing values.

init_cor_df <- na.omit(clean_disease)
init_Mcor <- init_cor_df[,2:30]
disease_cor_matrix <- vis_cor(init_Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")


cor_data_frame <- round(cor(init_Mcor),2)
# write.csv(cor_data_frame,"disease_correlation.csv", row.names = TRUE)

# We now want to remove

# crude_diabetes_prevalence_female (column 6)
# deaths_unsafe_sanitation_sex_both_age_70_number (column 10)
# deaths_unsafe_water_source_sex_both_age_70_number (column 11)

clean_disease <- subset(clean_disease, select = -c(6,10,11))


# We now want to impute our data using the MICE method.

set.seed(100)
disease_imputation <- mice(data = clean_disease, m = 5, method = c("pmm"), maxit = 100)

disease_1 <- complete(disease_imputation, 1)
disease_2 <- complete(disease_imputation, 2)
disease_3 <- complete(disease_imputation, 3)
disease_4 <- complete(disease_imputation, 4)
disease_5 <- complete(disease_imputation, 5)


write.csv(disease_1,"disease_1.csv", row.names = TRUE)
write.csv(disease_2,"disease_2.csv", row.names = TRUE)
write.csv(disease_3,"disease_3.csv", row.names = TRUE)
write.csv(disease_4,"disease_4.csv", row.names = TRUE)
write.csv(disease_5,"disease_5.csv", row.names = TRUE)





# We will now do model selection using the VIF function.
#write.csv(disease_1,"disease_for_rows.csv", row.names = TRUE)

full_model_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_1)

vif(full_model_1)

# We remove prevalence_of_obesity_both_sexes_who_2019 

d1_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_1)

vif(d1_1)

# We remove deaths_unsafe_sanitation_sex_both_age_70_rate


d1_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
              deaths_unsafe_water_source_sex_both_age_70_rate + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_1)

vif(d1_2)

# We remove deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate

d1_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_unsafe_water_source_sex_both_age_70_rate + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_1)

vif(d1_3)

# We remove age_standardised_diabetes_prevalence_female

d1_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_unsafe_water_source_sex_both_age_70_rate + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_1)

vif(d1_4)

# We remove prevalence_of_obesity_female_who_2019

d1_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_unsafe_water_source_sex_both_age_70_rate + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_5)

# We remove age_standardised_diabetes_prevalence_male

d1_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
             crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_unsafe_water_source_sex_both_age_70_rate + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_6)

# We remove deaths_unsafe_water_source_sex_both_age_70_rate

d1_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
             crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_7)

# We remove stroke_ihme_2017

d1_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
             crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_8)

# We remove ratio_of_diabetes_to_obesity_prevalence

d1_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
             crude_diabetes_prevalence_male  + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_9)

# We remove digestive_disease_ihme_2017

d1_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
             crude_diabetes_prevalence_male  + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + 
             dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_1)

vif(d1_10)

# We now have all our values under 5, so we can proceed with this model.

step_d1 <- step(d1_10)
summary(step_d1)

# We now repeat this for the second set of imputed data.


full_model_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_2)

vif(full_model_2)

# prevalence_of_obesity_both_sexes_who_2019


d2_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_2)

vif(d2_1)

# deaths_unsafe_sanitation_sex_both_age_70_rate

d2_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_2)

vif(d2_2)

# deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate

d2_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_2)

vif(d2_3)

#  age_standardised_diabetes_prevalence_female


d2_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_2)

vif(d2_4)

# prevalence_of_obesity_female_who_2019

d2_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_5)

# age_standardised_diabetes_prevalence_male

d2_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_6)

# deaths_unsafe_water_source_sex_both_age_70_rate

d2_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_7)

# stroke_ihme_2017

d2_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_8)

# digestive_disease_ihme_2017

d2_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_9)

# ratio_of_diabetes_to_obesity_prevalence

d2_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_2)

vif(d2_10)

# We now have all our values under 5, so we can proceed with this model.

step_d2 <- step(d2_10)
summary(step_d2)

# Third Dataset

full_model_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_3)

vif(full_model_3)

# prevalence_of_obesity_both_sexes_who_2019

d3_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_3)

vif(d3_1)

# deaths_unsafe_sanitation_sex_both_age_70_rate

d3_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_3)

vif(d3_2)

# deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate 

d3_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_3)


vif(d3_3)

# age_standardised_diabetes_prevalence_female

d3_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_3)


vif(d3_4)

# prevalence_of_obesity_female_who_2019

d3_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_5)

# age_standardised_diabetes_prevalence_male

d3_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_6)

# deaths_unsafe_water_source_sex_both_age_70_rate

d3_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_7)

# stroke_ihme_2017

d3_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_8)

# digestive_disease_ihme_2017

d3_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_9)

# ratio_of_diabetes_to_obesity_prevalence

d3_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_3)


vif(d3_10)

# We have all VIF values under five now, so can proceed.

step_d3 <- step(d3_10)
summary(step_d3)

# Fourth Dataset

full_model_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_4)

vif(full_model_4)

# prevalence_of_obesity_both_sexes_who_2019 

d4_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_4)

vif(d4_1)

# deaths_unsafe_sanitation_sex_both_age_70_rate

d4_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_4)

vif(d4_2)

# deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate

d4_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_4)

vif(d4_3)

# age_standardised_diabetes_prevalence_female

d4_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_4)

vif(d4_4)

# prevalence_of_obesity_female_who_2019

d4_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_4)

vif(d4_5)

# age_standardised_diabetes_prevalence_male

d4_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_4)

vif(d4_6)

# deaths_unsafe_water_source_sex_both_age_70_rate 

d4_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_4)

vif(d4_7)

# stroke_ihme_2017

d4_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_4)

vif(d4_8)

# digestive_disease_ihme_2017

d4_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_4)

vif(d4_9)

# prevalence_of_obesity_male_who_2019

d4_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017, data = disease_4)

vif(d4_10)

# ratio_of_diabetes_to_overweight_prevalence

d4_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              deaths_no_access_to_handwashing_facility_sex_both_age_70_number
            + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
              cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
              diabetes_blood_and_endocrine_disease_ihme_2017 + 
              hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
              parkinsons_disease_ihme_2017, data = disease_4)

vif(d4_11)

# We can now do step as we have all VIF values under five.

step_d4_11 <- step(d4_11)
summary(step_d4_11)

# We now do this for the final imputed dataset.

full_model_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + prevalence_of_obesity_both_sexes_who_2019 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(full_model_5)

# prevalence_of_obesity_both_sexes_who_2019

d5_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
                     age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
                     ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
                     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + 
                     deaths_unsafe_sanitation_sex_both_age_70_rate + deaths_unsafe_water_source_sex_both_age_70_rate 
                   + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
                     cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
                     respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
                     diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
                     hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
                     parkinsons_disease_ihme_2017 + 
                     prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(d5_1)

# deaths_unsafe_sanitation_sex_both_age_70_rate

d5_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + 
             deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(d5_2)

# deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate

d5_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(d5_3)

# age_standardised_diabetes_prevalence_female

d5_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + 
             age_standardised_diabetes_prevalence_female + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(d5_3)

# age_standardised_diabetes_prevalence_female

d5_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019 + prevalence_of_obesity_female_who_2019, data = disease_5)

vif(d5_4)

# prevalence_of_obesity_female_who_2019

d5_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~age_standardised_diabetes_prevalence_male + crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_5)

# age_standardised_diabetes_prevalence_male

d5_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number + deaths_unsafe_water_source_sex_both_age_70_rate 
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_6)

# deaths_unsafe_water_source_sex_both_age_70_rate

d5_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number  
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + stroke_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_7)

# stroke_ihme_2017

d5_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number  
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + digestive_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_8)

# digestive_disease_ihme_2017

d5_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
             ratio_of_diabetes_to_obesity_prevalence + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number  
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_9)

# ratio_of_diabetes_to_obesity_prevalence

d5_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + ratio_of_diabetes_to_overweight_prevalence +
             deaths_no_access_to_handwashing_facility_sex_both_age_70_number  
           + male_lung_cancer_deaths_per_100_000_who_iarc_2016 + dementia_ihme_2017 + 
             cardiovascular_diseases_ihme_2017 + kidney_disease_ihme_2017 +
             respiratory_disease_ihme_2017 + liver_disease_ihme_2017 + 
             diabetes_blood_and_endocrine_disease_ihme_2017 + 
             hepatitis_ihme_2017 + meningitis_ihme_2017 + cancers_ihme_2017 + 
             parkinsons_disease_ihme_2017 + 
             prevalence_of_obesity_male_who_2019, data = disease_5)

vif(d5_10)

# All of the VIF values are under five, so we have our final model.

step_d5_10 <- step(d5_10)
summary(step_d5_10)


























































































































































































































































