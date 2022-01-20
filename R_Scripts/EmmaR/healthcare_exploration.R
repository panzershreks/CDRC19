library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)

install.packages('naniar')
load.package(naniar)
library(naniar)



# Import data 

clean_healthcare <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_healthcare.csv")
clean_healthcare <- subset(clean_healthcare, select = -1)
clean_healthcare <- clean_names(clean_healthcare)

# View missing data
summary(clean_healthcare)

missing_table <- miss_var_summary(clean_healthcare, sort_miss = TRUE)
missing_table <- data.frame(missing_table)

for (i in 1:78){
  if (missing_table$pct_miss[i] > 50){
    print(missing_table$variable[i]) 
  } 
}

                                                           
drop <- c("percentage_of_persons_without_health_insurance_percent",
          "standard_deviation_of_life_satisfaction",
          "public_expenditure_on_health_tanzi_schuktnecht_2000",
          "hospital_beds_nurse_to_bed_ratio_oecd",
          "beds_in_not_for_profit_privately_owned_hospitals_number_oecd",
          "beds_in_not_for_profit_privately_owned_hospitals_per_1_000_population_oecd",
          "nurses_per_1_000_population_oecd",
          "nurses_headcount_oecd",
          "dentists_per_100_000_population_oecd",
          "dentists_headcount_oecd",
          "midwives_headcount_oecd",
          "midwives_per_1_000_live_births_oecd",
          "not_for_profit_privately_owned_hospitals_number_oecd",
          "not_for_profit_privately_owned_hospitals_per_million_population_oecd",
          "beds_in_for_profit_privately_owned_hospitals_number_oecd",
          "beds_in_for_profit_privately_owned_hospitals_per_1_000_population_oecd",
          "for_profit_privately_owned_hospitals_number_oecd",
          "for_profit_privately_owned_hospitals_per_million_population_oecd",
          "physicians_per_1_000_population_oecd",
          "physicians_headcount_oecd",
          "publicly_owned_hospitals_number_oecd",
          "publicly_owned_hospitals_per_million_population_oecd",
          "beds_in_publicly_owned_hospitals_number_oecd",
          "beds_in_publicly_owned_hospitals_per_1_000_population_oecd",
          "long_term_care_beds_number_oecd",
          "long_term_care_beds_per_1_000_population_oecd",
          "long_term_care_beds_per_1_000_population_aged_65_oecd",
          "general_hospitals_number_oecd",
          "general_hospitals_per_million_population_oecd",
          "hospitals_number_oecd",
          "hospitals_per_million_population_oecd",
          "health_expenditure_and_financing_per_capita_oec_dstat_2017",
          "surgical_specialists_per_1_000_population_oecd",
          "surgical_specialists_headcount_oecd",
          "health",
          "acute_care_beds_number_oecd",
          "acute_care_beds_per_1_000_population_oecd",
          "psychiatrists_per_1_000_population_oecd",
          "psychiatrists_headcount_oecd",
          "psychiatric_care_beds_number_oecd",
          "psychiatric_care_beds_per_1_000_population_oecd",
          "old_age",
          "how_much_we_think_we_spend_on_health_expenditure_ipsos_2016",
          "how_much_we_actually_spend_on_health_expenditure_ipsos_2016",
          "hospital_beds_number_oecd",
          "hospital_beds_per_1_000_population_oecd",
          "public_expenditure_on_health_per_capita_in_developing_countries_ppp_world_bank_wdi_2017") 
        

# Create 'healthcare_d1' without variables with > 50% missing data
healthcare_d1 <- clean_healthcare[,!(names(clean_healthcare) %in% drop)]

# View histogram of missing data
md.pattern(healthcare_d1)
aggr_plot <- aggr(healthcare_d1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(healthcare_d1), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Imputation of missing data in healthcare_d1

# MICE with random forest imputation method

# set.seed(100)
# temp_healthcare_d1 <- mice(data = healthcare_d1, m = 5, method = c("rf"), maxit=100)
# summary(temp_healthcare_d1)
# 
# completed_healthcare_d1 <- complete(temp_healthcare_d1,1)
# view(completed_healthcare_d1)
# 
# colnames(completed_healthcare_d1)
# 
# xyplot(temp_healthcare_d1, total_confirmed_deaths_due_to_covid_19_per_million_people ~ entity+
#          share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
#          share_of_people_who_agree_vaccines_are_safe+
#          share_of_people_who_disagree_vaccines_are_safe+
#          share_of_people_who_agree_vaccines_are_effective+
#          share_of_people_who_disagree_vaccines_are_effective+
#          share_of_people_who_agree_vaccines_are_important_for_children_to_have+
#          all_causes_disability_adjusted_life_years_who_2015+
#          share_of_population_covered_by_health_insurance_ilo_2014+
#          current_health_expenditure_per_capita_ppp_current_international+
#          health_expenditure_per_capita_ppp_world_bank_2016+
#          haq_index_ihme_2017+
#          healthy_life_expectancy_ihme+
#          life_expectancy_ihme+
#          years_lived_with_disability_ihme+
#          out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
#          public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
#          total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
#          bcg_immunization_coverage_among_1_year_olds_who_2017+
#          hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
#          dtp3_immunization_coverage_among_1_year_olds_who_2017+
#          polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
#          measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
#          number_of_confirmed_tetanus_cases_who_2017+
#          number_confirmed_polio_cases_who_2017+
#          number_of_confirmed_pertussis_cases_who_2017+
#          number_of_confirmed_measles_cases_who_2017+
#          number_of_confirmed_diphtheria_cases_who_2017+
#          estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
#          estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017,
#        pch=18,cex=1)

# We want to see is that the shape of the magenta points (imputed) matches the shape of the blue points (observed)

# MICE with Classification and regression trees imputation method

set.seed(100)
temp_healthcare_d1 <- mice(data = healthcare_d1, m = 5, method = c("cart"), maxit=100)
summary(temp_healthcare_d1)

completed_healthcare_d1 <- complete(temp_healthcare_d1,1)

xyplot(temp_healthcare_d1, total_confirmed_deaths_due_to_covid_19_per_million_people ~ entity+
         share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
         share_of_people_who_agree_vaccines_are_safe+
         share_of_people_who_disagree_vaccines_are_safe+
         share_of_people_who_agree_vaccines_are_effective+
         share_of_people_who_disagree_vaccines_are_effective+
         share_of_people_who_agree_vaccines_are_important_for_children_to_have+
         all_causes_disability_adjusted_life_years_who_2015+
         share_of_population_covered_by_health_insurance_ilo_2014+
         current_health_expenditure_per_capita_ppp_current_international+
         health_expenditure_per_capita_ppp_world_bank_2016+
         haq_index_ihme_2017+
         healthy_life_expectancy_ihme+
         life_expectancy_ihme+
         years_lived_with_disability_ihme+
         out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
         public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
         total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
         bcg_immunization_coverage_among_1_year_olds_who_2017+
         hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
         dtp3_immunization_coverage_among_1_year_olds_who_2017+
         polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
         measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
         number_of_confirmed_tetanus_cases_who_2017+
         number_confirmed_polio_cases_who_2017+
         number_of_confirmed_pertussis_cases_who_2017+
         number_of_confirmed_measles_cases_who_2017+
         number_of_confirmed_diphtheria_cases_who_2017+
         estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
         estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017,
       pch=18,cex=1)

# We want to see is that the shape of the magenta points (imputed) matches the shape of the blue points (observed)

# Density plot
densityplot(temp_healthcare_d1)

#Strip plot (returns empty plots)
stripplot(temp_healthcare_d1, pch = 20, cex = 1.2)

# Compute correlation 
corr_data <- completed_healthcare_d1[,2:31]

healthcare_corr <- vis_cor(corr_data) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

healthcare_corr_df <- round(cor(corr_data),2)

#write.csv(healthcare_corr_df,"healthcare_correlation_df.csv", row.names = TRUE)

# Linear models 

all_healthcare_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
  share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
  share_of_people_who_agree_vaccines_are_safe+
  share_of_people_who_disagree_vaccines_are_safe+
  share_of_people_who_agree_vaccines_are_effective+
  share_of_people_who_disagree_vaccines_are_effective+
  share_of_people_who_agree_vaccines_are_important_for_children_to_have+
  all_causes_disability_adjusted_life_years_who_2015+
  share_of_population_covered_by_health_insurance_ilo_2014+
  current_health_expenditure_per_capita_ppp_current_international+
  health_expenditure_per_capita_ppp_world_bank_2016+
  haq_index_ihme_2017+
  healthy_life_expectancy_ihme+
  life_expectancy_ihme+
  years_lived_with_disability_ihme+
  out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
  public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
  total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
  bcg_immunization_coverage_among_1_year_olds_who_2017+
  hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
  dtp3_immunization_coverage_among_1_year_olds_who_2017+
  polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
  measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
  number_of_confirmed_tetanus_cases_who_2017+
  number_confirmed_polio_cases_who_2017+
  number_of_confirmed_pertussis_cases_who_2017+
  number_of_confirmed_measles_cases_who_2017+
  number_of_confirmed_diphtheria_cases_who_2017+
  estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
  estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)


# # Create an empty list 
# model_summaries <- list()
# 
# # for loop to run multiple linear regression models where each loop adds a predictor 
# 
# for(i in 3:ncol(completed_healthcare_d1)) {               
#   predictors_i <- colnames(completed_healthcare_d1)[3:i]    
#   model_summaries[[i - 1]] <- summary(     
#     lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ ., completed_healthcare_d1[ , c("total_confirmed_deaths_due_to_covid_19_per_million_people", predictors_i)]))
#   
# }
# 
# model_summaries
# 
# # Note most of the R squared values are around 22-25%
# 
# # Using 5% significance level current_health_expenditure_per_capita_ppp_current_international                          
# share_of_people_who_agree_vaccines_are_effective                                         
# share_of_people_who_agree_vaccines_are_safe
# haq_index_ihme_2017                                                                      
# life_expectancy_ihme
# hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017
# 
# # Using model selection
# 
# df <- completed_healthcare_d1[, c("total_confirmed_deaths_due_to_covid_19_per_million_people",
#                              "share_of_people_who_disagree_vaccines_are_important_for_children_to_have",
#                              "share_of_people_who_agree_vaccines_are_safe",
#                              "share_of_people_who_disagree_vaccines_are_safe",
#                              "share_of_people_who_agree_vaccines_are_effective",
#                              "share_of_people_who_disagree_vaccines_are_effective",
#                              "share_of_people_who_agree_vaccines_are_important_for_children_to_have",
#                              "all_causes_disability_adjusted_life_years_who_2015",
#                              "share_of_population_covered_by_health_insurance_ilo_2014",
#                              "current_health_expenditure_per_capita_ppp_current_international",
#                              "health_expenditure_per_capita_ppp_world_bank_2016",
#                              "haq_index_ihme_2017",
#                              "healthy_life_expectancy_ihme",
#                              "life_expectancy_ihme",
#                              "years_lived_with_disability_ihme",
#                              "out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure",
#                              "public_expenditure_on_health_percent_gdp_owid_extrapolated_series",
#                              "total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors",
#                              "bcg_immunization_coverage_among_1_year_olds_who_2017",
#                              "hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017",
#                              "dtp3_immunization_coverage_among_1_year_olds_who_2017",
#                              "polio_pol3_immunization_coverage_among_1_year_olds_who_2017",
#                              "measles_mcv_immunization_coverage_among_1_year_olds_who_2017",
#                              "number_of_confirmed_tetanus_cases_who_2017",
#                              "number_confirmed_polio_cases_who_2017",
#                              "number_of_confirmed_pertussis_cases_who_2017",
#                              "number_of_confirmed_measles_cases_who_2017",
#                              "number_of_confirmed_diphtheria_cases_who_2017",
#                              "estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017",
#                              "estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017")]
# 
# full.model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
#                    share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
#                    share_of_people_who_agree_vaccines_are_safe+
#                    share_of_people_who_disagree_vaccines_are_safe+
#                    share_of_people_who_agree_vaccines_are_effective+
#                    share_of_people_who_disagree_vaccines_are_effective+
#                    share_of_people_who_agree_vaccines_are_important_for_children_to_have+
#                    all_causes_disability_adjusted_life_years_who_2015+
#                    share_of_population_covered_by_health_insurance_ilo_2014+
#                    current_health_expenditure_per_capita_ppp_current_international+
#                    health_expenditure_per_capita_ppp_world_bank_2016+
#                    haq_index_ihme_2017+
#                    healthy_life_expectancy_ihme+
#                    life_expectancy_ihme+
#                    years_lived_with_disability_ihme+
#                    out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
#                    public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
#                    total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
#                    bcg_immunization_coverage_among_1_year_olds_who_2017+
#                    hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
#                    dtp3_immunization_coverage_among_1_year_olds_who_2017+
#                    polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
#                    measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
#                    number_of_confirmed_tetanus_cases_who_2017+
#                    number_confirmed_polio_cases_who_2017+
#                    number_of_confirmed_pertussis_cases_who_2017+
#                    number_of_confirmed_measles_cases_who_2017+
#                    number_of_confirmed_diphtheria_cases_who_2017+
#                    estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
#                    estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, 
#                  data = df, 
#                  na.action = "na.fail")
# 
# 
# dredge(full.model, beta = "none", evaluate = TRUE, rank = "AIC")


# help(dredge)

# VIF 

full_healthcare_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                             share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                             share_of_people_who_agree_vaccines_are_safe+
                             share_of_people_who_disagree_vaccines_are_safe+
                             share_of_people_who_agree_vaccines_are_effective+
                             share_of_people_who_disagree_vaccines_are_effective+
                             share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                             all_causes_disability_adjusted_life_years_who_2015+
                             share_of_population_covered_by_health_insurance_ilo_2014+
                             current_health_expenditure_per_capita_ppp_current_international+
                             health_expenditure_per_capita_ppp_world_bank_2016+
                             haq_index_ihme_2017+
                             healthy_life_expectancy_ihme+
                             life_expectancy_ihme+
                             years_lived_with_disability_ihme+
                             out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                             public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                             total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                             bcg_immunization_coverage_among_1_year_olds_who_2017+
                             hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                             dtp3_immunization_coverage_among_1_year_olds_who_2017+
                             polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                             measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                             number_of_confirmed_tetanus_cases_who_2017+
                             number_confirmed_polio_cases_who_2017+
                             number_of_confirmed_pertussis_cases_who_2017+
                             number_of_confirmed_measles_cases_who_2017+
                             number_of_confirmed_diphtheria_cases_who_2017+
                             estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                             estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(full_healthcare_model)

healthcare_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                              share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                              share_of_people_who_agree_vaccines_are_safe+
                              share_of_people_who_disagree_vaccines_are_safe+
                              share_of_people_who_agree_vaccines_are_effective+
                              share_of_people_who_disagree_vaccines_are_effective+
                              share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                              all_causes_disability_adjusted_life_years_who_2015+
                              share_of_population_covered_by_health_insurance_ilo_2014+
                              current_health_expenditure_per_capita_ppp_current_international+
                              health_expenditure_per_capita_ppp_world_bank_2016+
                              haq_index_ihme_2017+
                              healthy_life_expectancy_ihme+
                              years_lived_with_disability_ihme+
                              out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                              public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                              total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                              bcg_immunization_coverage_among_1_year_olds_who_2017+
                              hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                              dtp3_immunization_coverage_among_1_year_olds_who_2017+
                              polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                              measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                              number_of_confirmed_tetanus_cases_who_2017+
                              number_confirmed_polio_cases_who_2017+
                              number_of_confirmed_pertussis_cases_who_2017+
                              number_of_confirmed_measles_cases_who_2017+
                              number_of_confirmed_diphtheria_cases_who_2017+
                              estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                              estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_1)

healthcare_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_agree_vaccines_are_safe+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     haq_index_ihme_2017+
                     healthy_life_expectancy_ihme+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     dtp3_immunization_coverage_among_1_year_olds_who_2017+
                     polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_2)

healthcare_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_agree_vaccines_are_safe+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     haq_index_ihme_2017+
                     healthy_life_expectancy_ihme+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_3)

healthcare_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_agree_vaccines_are_safe+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     healthy_life_expectancy_ihme+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_4)

healthcare_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     healthy_life_expectancy_ihme+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     polio_pol3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_5)

healthcare_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     healthy_life_expectancy_ihme+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_6)

healthcare_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     share_of_people_who_agree_vaccines_are_important_for_children_to_have+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_7)

healthcare_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017+
                     estimated_number_of_deaths_due_to_tuberculosis_excluding_hiv_who_2017, data = completed_healthcare_d1)

vif(healthcare_8)

healthcare_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     hepatitis_b_hep_b3_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                    data = completed_healthcare_d1)

vif(healthcare_9)

healthcare_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                     share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                     share_of_people_who_disagree_vaccines_are_safe+
                     share_of_people_who_agree_vaccines_are_effective+
                     share_of_people_who_disagree_vaccines_are_effective+
                     all_causes_disability_adjusted_life_years_who_2015+
                     share_of_population_covered_by_health_insurance_ilo_2014+
                     health_expenditure_per_capita_ppp_world_bank_2016+
                     years_lived_with_disability_ihme+
                     out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                     public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                     total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                     bcg_immunization_coverage_among_1_year_olds_who_2017+
                     measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                     number_of_confirmed_tetanus_cases_who_2017+
                     number_confirmed_polio_cases_who_2017+
                     number_of_confirmed_pertussis_cases_who_2017+
                     number_of_confirmed_measles_cases_who_2017+
                     number_of_confirmed_diphtheria_cases_who_2017+
                     estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                   data = completed_healthcare_d1)

vif(healthcare_10)

healthcare_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                      share_of_people_who_disagree_vaccines_are_important_for_children_to_have+
                      share_of_people_who_disagree_vaccines_are_safe+
                      share_of_people_who_agree_vaccines_are_effective+
                      share_of_people_who_disagree_vaccines_are_effective+
                      share_of_population_covered_by_health_insurance_ilo_2014+
                      health_expenditure_per_capita_ppp_world_bank_2016+
                      years_lived_with_disability_ihme+
                      out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure+
                      public_expenditure_on_health_percent_gdp_owid_extrapolated_series+
                      total_gross_official_disbursements_for_medical_research_and_basic_heath_sectors+
                      bcg_immunization_coverage_among_1_year_olds_who_2017+
                      measles_mcv_immunization_coverage_among_1_year_olds_who_2017+
                      number_of_confirmed_tetanus_cases_who_2017+
                      number_confirmed_polio_cases_who_2017+
                      number_of_confirmed_pertussis_cases_who_2017+
                      number_of_confirmed_measles_cases_who_2017+
                      number_of_confirmed_diphtheria_cases_who_2017+
                      estimated_deaths_due_to_tuberculosis_per_100_000_population_excluding_hiv_who_2017,
                    data = completed_healthcare_d1)

vif(healthcare_11)

step_healthcare <- step(healthcare_11)
summary(step_healthcare)

