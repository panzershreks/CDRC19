
# GLM Models with MissForest Imputation
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
![FullModImp](FullModImp.png)

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
(Intercept)                                                                               3.896e+00  8.46e-08 ***
income_support1                                                                           7.060e-01  0.002524 ** 
income_support2                                                                           3.722e-01  0.220276    
age_standardised_diabetes_prevalence_female                                              -7.199e-02  0.042662 *  
prevalence_of_obesity_both_sexes_who_2019                                                 6.108e-02  0.000325 ***
income_classification_world_bank_20172                                                    7.510e-01  0.032826 *  
income_classification_world_bank_20173                                                    1.029e+00  0.018439 *  
income_classification_world_bank_20174                                                    6.768e-01  0.208428    
all_causes_disability_adjusted_life_years_who_2015                                       -1.885e-05  0.011751 *  
hospital_beds_per_1_000_population_oecd                                                  -1.564e-01  0.046807 *  
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure  7.722e-04  0.063790 .  
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
(Intercept)                                                                               5.097e+00  1.90e-09 ***
income_support1                                                                           5.358e-01  0.013036 *  
income_support2                                                                           2.910e-01  0.374284    
age_standardised_diabetes_prevalence_female                                              -1.347e-01  0.000239 ***
prevalence_of_obesity_both_sexes_who_2019                                                 8.202e-02  8.48e-07 ***
income_classification_world_bank_20172                                                    5.204e-01  0.112499    
income_classification_world_bank_20173                                                    7.293e-01  0.072952 .  
income_classification_world_bank_20174                                                    2.123e-01  0.665380    
all_causes_disability_adjusted_life_years_who_2015                                       -1.726e-05  0.014774 *  
nurses_per_1_000_population_oecd                                                         -7.578e-02  0.030762 *  
hospital_beds_per_1_000_population_oecd                                                  -1.573e-01  0.021940 *  
out_of_pocket_expenditure_per_capita_on_healthcare_ppp_usd_who_global_health_expenditure  9.490e-04  0.020243 * 
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
age_standardised_diabetes_prevalence_male/age_standardised_diabetes_prevalence_female

```

## Variables in 2 models
```

income_classification_world_bank_2017
all_causes_disability_adjusted_life_years_who_2015
hospital_beds_per_1_000_population_oecd
nurses_per_1_000_population
```