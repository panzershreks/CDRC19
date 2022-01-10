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


clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -1)

missing_table <- miss_var_summary(clean_disease, sort_miss = TRUE)
# vis_miss(clean_disease)
missing_table
# write.csv(missing_table, file = "disease_missing.csv", row.names = TRUE)

# We now want to do imputation

set.seed(100)
disease_imputation <- mice(data = clean_disease, m = 5, method = c("cart"), maxit = 100)

disease_imputation$loggedEvents

# We have that there are a few constant/collinear columns, so we will now deal with them
# by removing them.

disease_1 <- complete(disease_imputation, 1)
disease_2 <- complete(disease_imputation, 2)
disease_3 <- complete(disease_imputation, 3)
disease_4 <- complete(disease_imputation, 4)
disease_5 <- complete(disease_imputation, 5)

# we will from now on work with only the first imputed version of events.

working_disease <- disease_1

# There is still some missing data -

miss_var_summary(working_disease)
# vis_miss(working_disease)
nrow(working_disease)
working_disease <- na.omit(working_disease)
miss_var_summary(working_disease)
nrow(working_disease)



# now do correlation between values
# then VIF
# then model.

Mcor <- working_disease[,2:30]

disease_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

cor_data_frame <- round(cor(Mcor),2)

#write.csv(cor_data_frame,"disease_correlation.csv", row.names = TRUE)



# Now you want to use VIF - we will remove the largest valued variable each time
# until we have that all the values are under five.



full_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
                 age_standardised_diabetes_prevalence_female +
                 crude_diabetes_prevalence_male + 
                   ratio_of_diabetes_to_obesity_prevalence +
                 ratio_of_diabetes_to_overweight_prevalence +
                 deaths_unsafe_sanitation_sex_both_age_70_number +
                 deaths_unsafe_water_source_sex_both_age_70_number +
                 deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate +
                 deaths_unsafe_sanitation_sex_both_age_70_rate +
                 deaths_unsafe_water_source_sex_both_age_70_rate +
                 male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
                 dementia_ihme_2017 +
                 cardiovascular_diseases_ihme_2017 +
                 stroke_ihme_2017 +
                 kidney_disease_ihme_2017 +
                 respiratory_disease_ihme_2017 +
                 liver_disease_ihme_2017 +
                 diabetes_blood_and_endocrine_disease_ihme_2017 +
                 digestive_disease_ihme_2017 +
                 hepatitis_ihme_2017 +
                 meningitis_ihme_2017 +
                 cancers_ihme_2017 +
                 parkinsons_disease_ihme_2017 +
                 prevalence_of_obesity_both_sexes_who_2019 +
                 prevalence_of_obesity_male_who_2019 +
                 prevalence_of_obesity_female_who_2019, data = working_disease)
vif(full_model)

# We remove deaths_unsafe_water_source_sex_both_age_70_number

dis_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
     age_standardised_diabetes_prevalence_female +
     crude_diabetes_prevalence_male + 
     ratio_of_diabetes_to_obesity_prevalence +
     ratio_of_diabetes_to_overweight_prevalence +
     deaths_unsafe_sanitation_sex_both_age_70_number +
     deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate +
     deaths_unsafe_sanitation_sex_both_age_70_rate +
     deaths_unsafe_water_source_sex_both_age_70_rate +
     male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
     dementia_ihme_2017 +
     cardiovascular_diseases_ihme_2017 +
     stroke_ihme_2017 +
     kidney_disease_ihme_2017 +
     respiratory_disease_ihme_2017 +
     liver_disease_ihme_2017 +
     diabetes_blood_and_endocrine_disease_ihme_2017 +
     digestive_disease_ihme_2017 +
     hepatitis_ihme_2017 +
     meningitis_ihme_2017 +
     cancers_ihme_2017 +
     parkinsons_disease_ihme_2017 +
     prevalence_of_obesity_both_sexes_who_2019 +
     prevalence_of_obesity_male_who_2019 +
     prevalence_of_obesity_female_who_2019, data = working_disease)

vif(dis_1)


# We now remove prevalence_of_obesity_both_sexes_who_2019

dis_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              age_standardised_diabetes_prevalence_female +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate +
              deaths_unsafe_sanitation_sex_both_age_70_rate +
              deaths_unsafe_water_source_sex_both_age_70_rate +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019 +
              prevalence_of_obesity_female_who_2019, data = working_disease)

vif(dis_2)

# We now remove deaths_unsafe_sanitation_sex_both_age_70_rate

dis_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              age_standardised_diabetes_prevalence_female +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate +
              deaths_unsafe_water_source_sex_both_age_70_rate +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019 +
              prevalence_of_obesity_female_who_2019, data = working_disease)

vif(dis_3)

# We now remove deaths_no_access_to_handwashing_facility_sex_both_age_70_r_ate 

dis_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              age_standardised_diabetes_prevalence_female +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              deaths_unsafe_water_source_sex_both_age_70_rate +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019 +
              prevalence_of_obesity_female_who_2019, data = working_disease)

vif(dis_4)

# We now remove age_standardised_diabetes_prevalence_female

dis_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              deaths_unsafe_water_source_sex_both_age_70_rate +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019 +
              prevalence_of_obesity_female_who_2019, data = working_disease)

vif(dis_5)

# We now remove prevalence_of_obesity_female_who_2019 

dis_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              deaths_unsafe_water_source_sex_both_age_70_rate +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_6)

# We now remove deaths_unsafe_water_source_sex_both_age_70_rate

dis_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ age_standardised_diabetes_prevalence_male +
              crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_7)

# We now remove age_standardised_diabetes_prevalence_male 

dis_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              stroke_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_8)

# We now remove stroke_ihme_2017 

dis_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male + 
              ratio_of_diabetes_to_obesity_prevalence +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_9)

# We now remove ratio_of_diabetes_to_obesity_prevalence 

dis_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male +
              ratio_of_diabetes_to_overweight_prevalence +
              deaths_unsafe_sanitation_sex_both_age_70_number +
              male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
              dementia_ihme_2017 +
              cardiovascular_diseases_ihme_2017 +
              kidney_disease_ihme_2017 +
              respiratory_disease_ihme_2017 +
              liver_disease_ihme_2017 +
              diabetes_blood_and_endocrine_disease_ihme_2017 +
              digestive_disease_ihme_2017 +
              hepatitis_ihme_2017 +
              meningitis_ihme_2017 +
              cancers_ihme_2017 +
              parkinsons_disease_ihme_2017 +
              prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_10)

# We now remove digestive_disease_ihme_2017 

dis_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male +
               ratio_of_diabetes_to_overweight_prevalence +
               deaths_unsafe_sanitation_sex_both_age_70_number +
               male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
               dementia_ihme_2017 +
               cardiovascular_diseases_ihme_2017 +
               kidney_disease_ihme_2017 +
               respiratory_disease_ihme_2017 +
               liver_disease_ihme_2017 +
               diabetes_blood_and_endocrine_disease_ihme_2017 +
               hepatitis_ihme_2017 +
               meningitis_ihme_2017 +
               cancers_ihme_2017 +
               parkinsons_disease_ihme_2017 +
               prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_11)

# We now remove meningitis_ihme_2017 

dis_12 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ crude_diabetes_prevalence_male +
               ratio_of_diabetes_to_overweight_prevalence +
               deaths_unsafe_sanitation_sex_both_age_70_number +
               male_lung_cancer_deaths_per_100_000_who_iarc_2016 +
               dementia_ihme_2017 +
               cardiovascular_diseases_ihme_2017 +
               kidney_disease_ihme_2017 +
               respiratory_disease_ihme_2017 +
               liver_disease_ihme_2017 +
               diabetes_blood_and_endocrine_disease_ihme_2017 +
               hepatitis_ihme_2017 +
               cancers_ihme_2017 +
               parkinsons_disease_ihme_2017 +
               prevalence_of_obesity_male_who_2019, data = working_disease)

vif(dis_12)

# We now have all the values under five, so we can say all these variables deserve to be in the model.

# Now we will ues the step function to find significant variables.

step_disease <- step(dis_12)
summary(step_disease)
plot(step_disease)








