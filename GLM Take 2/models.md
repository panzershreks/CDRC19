## For "Full Model Imputation"

1. "junk" variables in each category was removed
2. categories were combined
3. imputed using random forest
4. removed other variables with poor interpretation
4. variables were iteratively removed till all had a VIF < 5
5. backwards selection was performed using AICc

Model Summary
```
                                                                                           Estimate  Pr(>|t|)    
(Intercept)                                                                               2.182e+00  0.004226 ** 
population_with_access_to_improved_sanitation_y                                           2.162e-09  0.032484 *  
age_standardised_diabetes_prevalence_male                                                -9.137e-02  0.029768 *  
prevalence_of_obesity_both_sexes_who_2019                                                 8.000e-02  1.18e-06 ***
life_satisfaction_in_cantril_ladder_world_happiness_report_2019                           3.360e-01  0.009110 ** 
income_support1                                                                           8.032e-01  0.000337 ***
income_support2                                                                           3.072e-01  0.355133    
nurses_per_1_000_population_oecd                                                         -8.760e-02  0.017627 *  
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure  9.922e-04  0.010484 *  
```

Relevant diagnostic plots:
![fullModImp](fullModImp.png)

Plot of fitted values:
![fullModImpFitted](fullModImpFitted.png)

## For "Categories Imputed Model"
1. each category was imputed separately using random forest
2. variables with poor interpretation were removed
3. variables in each category were iteratively removed till all had a VIF < 5
4. backwards selection was performed using AICc in each category
5. this yielded a set of "significant variables" in each category
6. the imputed data for these variables were combined into a single dataframe
7. variables were iteratively removed till all had a VIF < 5
8. backwards selection was performed using AICc

Model Summary
```
                                                                                           Estimate  Pr(>|t|)    
(Intercept)                                                                               3.172e+00  2.77e-06 ***
income_support1                                                                           5.714e-01  0.008969 ** 
income_support2                                                                          -1.161e-01  0.724960    
prevalence_of_obesity_both_sexes_who_2019                                                 4.999e-02  0.000347 ***
income_classification_world_bank_20172                                                    7.051e-01  0.033445 *  
income_classification_world_bank_20173                                                    9.635e-01  0.019729 *  
income_classification_world_bank_20174                                                    2.677e-01  0.617442    
national_poverty_lines_jolliffe_and_prydz_2016                                            5.860e-02  0.007239 ** 
all_causes_disability_adjusted_life_years_who_2015                                       -1.762e-05  0.015503 *  
hospital_beds_per_1_000_population_oecd                                                  -1.365e-01  0.068089 .  
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure  9.143e-04  0.023089 *  
```

Relevant diagnostic plots:
![catImpMod](catImpMod.png)

Plot of fitted values:
![catImpModFitted](catImpModFitted.png)

## For "Categories Miss Model"
1. each category was imputed separately using random forest
2. variables with poor interpretation were removed
3. variables in each category were iteratively removed till all had a VIF < 5
4. backwards selection was performed using AICc in each category
5. this yielded a set of "significant variables" in each category
6. the data for these variables (before imputation) were combined into a single dataframe
7. data was imputed using random forest
8. variables were iteratively removed till all had a VIF < 5
9. backwards selection was performed using AICc

Model Summary
```
                                                                                           Estimate  Pr(>|t|)    
(Intercept)                                                                               4.280e+00  3.02e-09 ***
income_support1                                                                           5.839e-01   0.00634 ** 
income_support2                                                                           1.406e-01   0.66896    
age_standardised_diabetes_prevalence_female                                              -1.131e-01   0.00109 ** 
prevalence_of_obesity_both_sexes_who_2019                                                 8.351e-02  5.45e-07 ***
income_classification_world_bank_20172                                                    5.065e-01   0.12030    
income_classification_world_bank_20173                                                    6.299e-01   0.11719    
income_classification_world_bank_20174                                                    8.038e-02   0.86886    
all_causes_disability_adjusted_life_years_who_2015                                       -1.808e-05   0.00940 ** 
hospital_beds_per_1_000_population_oecd                                                  -1.406e-01   0.03322 *  
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure  7.464e-04   0.05606 .  
```

Relevant diagnostic plots:
![catMisMod](catMisMod.png)

Plot of fitted values:
![catMisModFitted](catMisModFitted.png)

## Variables in all 3 models
```
income_support
prevalence_of_obesity_both_sexes_who_2019
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure
```

## Variables in 2 models
```
age_standardised_diabetes_prevalence_male/age_standardised_diabetes_prevalence_female
income_classification_world_bank_2017
all_causes_disability_adjusted_life_years_who_2015
hospital_beds_per_1_000_population_oecd
```