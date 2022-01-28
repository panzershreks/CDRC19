library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)
library(readr)
library(ggplot2)
library(naniar)
library(visdat)

# Import data 
clean_healthcare <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_healthcare.csv")
clean_healthcare <- subset(clean_healthcare, select = -1)
clean_healthcare <- clean_names(clean_healthcare)

# View missing data
summary(clean_healthcare)

missing_table <- miss_var_summary(clean_healthcare, sort_miss = TRUE)
missing_table <- data.frame(missing_table)
missing_table

# View variables with more than 50% of missing data
for (i in 1:78){
  if (missing_table$pct_miss[i] > 50){
    print(missing_table$variable[i]) 
  } 
}


# Dropping data below
# Following have more than 50% of missing data (and another reason for dropping data) 

drop <- c("percentage_of_persons_without_health_insurance_percent", #including share_of_population_covered_by_health_insurance_ilo_2014
          "standard_deviation_of_life_satisfaction", # more than 50% missing data and seems irrelevant
          "public_expenditure_on_health_tanzi_schuktnecht_2000", # including health_expenditure_per_capita_ppp_world_bank_2016
          "hospital_beds_nurse_to_bed_ratio_oecd", # including beds and nurses
          "beds_in_not_for_profit_privately_owned_hospitals_number_oecd",# including beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd
          "nurses_headcount_oecd", # including nurses_per_1_000_population_oecd
          "dentists_headcount_oecd", # including dentists_per_100_000_population_oecd
          "midwives_headcount_oecd", # including midwives_per_1_000_live_births_oecd
          "not_for_profit_privately_owned_hospitals_number_oecd", # including not_for_profit_privately_owned_hospitals_per_million_population_oecd
          "beds_in_for_profit_privately_owned_hospitals_number_oecd", # including beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd
          "for_profit_privately_owned_hospitals_number_oecd", # including for_profit_privately_owned_hospitals_per_million_population_oecd
          "physicians_headcount_oecd", # including physicians_per_1_000_population_oecd
          "publicly_owned_hospitals_number_oecd", # including publicly_owned_hospitals_per_million_population_oecd
          "beds_in_publicly_owned_hospitals_number_oecd", # including beds_in_publicly_owned_hospitals_per_1_000_population_oecd
          "long_term_care_beds_number_oecd", # including long_term_care_beds_per_1_000_population_oecd and long_term_care_beds_per_1_000_population_aged_65_oecd
          "general_hospitals_number_oecd", # including general_hospitals_per_million_population_oecd
          "hospitals_number_oecd", # including hospitals_per_million_population_oecd
          "health_expenditure_and_financing_per_capita_oec_dstat_2017",
          "surgical_specialists_headcount_oecd", # including surgical_specialists_per_1_000_population_oecd
          "health",
          "acute_care_beds_number_oecd", # including acute_care_beds_per_1_000_population_oecd
          "psychiatrists_headcount_oecd", # including psychiatrists_per_1_000_population_oecd
          "psychiatric_care_beds_number_oecd", # including psychiatric_care_beds_per_1_000_population_oecd
          "old_age",
          "how_much_we_think_we_spend_on_health_expenditure_ipsos_2016",
          "how_much_we_actually_spend_on_health_expenditure_ipsos_2016",
          "hospital_beds_number_oecd") # including hospital_beds_per_1_000_population_oecd


### The following data has over 50% of missing data but has been decided to keep ###
# beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd
# nurses_per_1_000_population_oecd
# dentists_per_100_000_population_oecd
# midwives_per_1_000_live_births_oecd
# not_for_profit_privately_owned_hospitals_per_million_population_oecd
# beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd
# for_profit_privately_owned_hospitals_per_million_population_oecd
# physicians_per_1_000_population_oecd
# publicly_owned_hospitals_per_million_population_oecd
# beds_in_publicly_owned_hospitals_per_1_000_population_oecd
# long_term_care_beds_per_1_000_population_oecd
# long_term_care_beds_per_1_000_population_aged_65_oecd
# general_hospitals_per_million_population_oecd
# hospitals_per_million_population_oecd
# surgical_specialists_per_1_000_population_oecd
# acute_care_beds_per_1_000_population_oecd
# psychiatrists_per_1_000_population_oecd
# psychiatric_care_beds_per_1_000_population_oecd
# hospital_beds_per_1_000_population_oecd



# Create 'healthcare_d1' without variables with > 50% missing data + justified reason for removing
healthcare_primary <- clean_healthcare[,!(names(clean_healthcare) %in% drop)]

# Imputation of missing data in healthcare_d1 using MICE with CART method

set.seed(100)

temp_healthcare_primary <- mice(data = healthcare_primary, m = 5, method = c("cart"), maxit=100)

summary(temp_healthcare_primary)

completed_healthcare_primary1 <- complete(temp_healthcare_primary,1)
completed_healthcare_primary2 <- complete(temp_healthcare_primary,2)
completed_healthcare_primary3 <- complete(temp_healthcare_primary,3)
completed_healthcare_primary4 <- complete(temp_healthcare_primary,4)
completed_healthcare_primary5 <- complete(temp_healthcare_primary,5)

missing_table1 <- miss_var_summary(completed_healthcare_primary1, sort_miss = TRUE)
missing_table1 <- data.frame(missing_table1)
missing_table1

missing_table2 <- miss_var_summary(completed_healthcare_primary2, sort_miss = TRUE)
missing_table2 <- data.frame(missing_table2)

missing_table3 <- miss_var_summary(completed_healthcare_primary3, sort_miss = TRUE)
missing_table3 <- data.frame(missing_table3)
missing_table3

missing_table4 <- miss_var_summary(completed_healthcare_primary4, sort_miss = TRUE)
missing_table4 <- data.frame(missing_table4)
missing_table4

missing_table5 <- miss_var_summary(completed_healthcare_primary5, sort_miss = TRUE)
missing_table5 <- data.frame(missing_table5)
missing_table5

temp_healthcare_primary


temp_healthcare_primary2 <- mice(data = completed_healthcare_primary, m = 5, method = c("cart"), maxit=100)

completed_healthcare_primary2 <- complete(temp_healthcare_primary2,1)

missing_table3 <- miss_var_summary(completed_healthcare_primary2, sort_miss = TRUE)
missing_table3 <- data.frame(missing_table3)
missing_table3

completed_healthcare_primary2

# Plot with imputed data
densityplot(temp_healthcare_primary)

# Compute correlation 

corr_data <- completed_healthcare_primary[,2:51]  

healthcare_corr <- vis_cor(corr_data) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

healthcare_corr_df <- round(cor(corr_data),2)

# Remove any variables with corr = 1

# VIF 



