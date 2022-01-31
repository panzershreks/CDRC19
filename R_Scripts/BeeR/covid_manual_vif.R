rf_covid_fm <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                    income_support+debt_relief+containment_index+
                    population_with_access_to_improved_sanitation_x+
                    population_without_access_to_improved_sanitation_x+
                    daily_new_confirmed_cases_of_covid_19+
                    daily_new_confirmed_deaths_due_to_covid_19+
                    total_confirmed_cases_of_covid_19+
                    total_confirmed_deaths_due_to_covid_19+
                    daily_new_confirmed_cases_of_covid_19_per_million_people+
                    daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                    total_confirmed_cases_of_covid_19_per_million_people+
                    days_since_the_total_confirmed_cases_of_covid_19_reached_100+
                    days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                    days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                    days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                    case_fatality_rate_of_covid_19+case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                    days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                    days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                    days_since_3_daily_new_confirmed_deaths_recorded+
                    daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                    daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned+
                    days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                    days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                    daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                    daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                    daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                    daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                    days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                    days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                    days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                    daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                    daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                    days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                    doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                    doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                    weekly_cases+weekly_deaths+weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_deaths+biweekly_case_growth+
                    biweekly_death_growth+weekly_cases_per_million_people+weekly_deaths_per_million_people+
                    biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                    case_fatality_rate_of_covid_19_short_term,data = covid_data)
alias(rf_covid_fm)
alias_1 <- as.matrix(alias(rf_covid_fm))
vif(rf_covid_fm)
max(vif(rf_covid_fm))
# take out highly correlated variables
# 4 taken out are weekly_cases, weekly_deaths, case_fatality_rate_of_covid_19 and days_since_the_total_confirmed_cases_of_covid_19_reached_100
rf_covid_fm_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_deaths+biweekly_case_growth+
                      biweekly_death_growth+weekly_cases_per_million_people+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
alias(rf_covid_fm_1)
alias_2 <- as.matrix(alias(rf_covid_fm_1))
vif(rf_covid_fm_1)
max(vif(rf_covid_fm_1))
# from VIF take out weekly_cases_per_million_people
rf_covid_fm_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_deaths+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_2)
max(vif(rf_covid_fm_2))
# from VIF take out daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned
rf_covid_fm_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_deaths+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_3)
max(vif(rf_covid_fm_3))
# from VIF take out daily_new_confirmed_cases_due_to_covid_19_rolling_7_day_average_right_aligned 
rf_covid_fm_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_deaths+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_4)
max(vif(rf_covid_fm_4))
# from VIF take out biweekly_deaths
rf_covid_fm_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_cases+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_5)
max(vif(rf_covid_fm_5))
# from VIF take out biweekly_cases
rf_covid_fm_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_6)
max(vif(rf_covid_fm_6))
# from VIF take out daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned 
rf_covid_fm_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+
                      biweekly_cases_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_7)
max(vif(rf_covid_fm_7))
# from VIF take out biweekly_cases_per_million_people 
rf_covid_fm_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_cases_of_covid_19+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_8)
max(vif(rf_covid_fm_8))
# from VIF take out daily_new_confirmed_cases_of_covid_19
rf_covid_fm_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                      income_support+debt_relief+containment_index+
                      population_with_access_to_improved_sanitation_x+
                      population_without_access_to_improved_sanitation_x+
                      daily_new_confirmed_deaths_due_to_covid_19+
                      total_confirmed_cases_of_covid_19+
                      total_confirmed_deaths_due_to_covid_19+
                      daily_new_confirmed_cases_of_covid_19_per_million_people+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                      total_confirmed_cases_of_covid_19_per_million_people+
                      days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                      days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                      days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                      case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                      days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                      days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                      days_since_3_daily_new_confirmed_deaths_recorded+
                      days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                      daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                      days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                      days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                      daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                      days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                      doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                      doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                      weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                      biweekly_death_growth+weekly_deaths_per_million_people+biweekly_deaths_per_million_people+
                      case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_9)
max(vif(rf_covid_fm_9))
# from VIF take out weekly_deaths_per_million_people
rf_covid_fm_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       daily_new_confirmed_deaths_due_to_covid_19+
                       total_confirmed_cases_of_covid_19+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_10)
max(vif(rf_covid_fm_10))
# from VIF take out daily_new_confirmed_deaths_due_to_covid_19
rf_covid_fm_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_cases_of_covid_19+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_11)
max(vif(rf_covid_fm_11))
# from VIF take out daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_3_day_average_right_aligned
rf_covid_fm_12 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_cases_of_covid_19+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_12)
max(vif(rf_covid_fm_12))
# from VIF take out days_since_daily_new_confirmed_cases_of_covid_19_rolling_7_day_average_right_aligned_reached_30 
rf_covid_fm_13 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_cases_of_covid_19+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_13)
max(vif(rf_covid_fm_13))
# from VIF take out total_confirmed_cases_of_covid_19 
rf_covid_fm_14 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+doubling_days_of_total_confirmed_deaths_7_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_14)
max(vif(rf_covid_fm_14))
# from VIF take out doubling_days_of_total_confirmed_deaths_7_day_period
rf_covid_fm_15 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_15)
max(vif(rf_covid_fm_15))
# from VIF take out daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_3_day_average_right_aligned 
rf_covid_fm_16 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_16)
max(vif(rf_covid_fm_16))
# from VIF take out days_since_the_total_confirmed_deaths_of_covid_19_per_million_people_reached_0_1 
rf_covid_fm_17 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+doubling_days_of_total_confirmed_cases_7_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_17)
max(vif(rf_covid_fm_17))
# from VIF take out doubling_days_of_total_confirmed_cases_7_day_period 
rf_covid_fm_18 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_18)
max(vif(rf_covid_fm_18))
# from VIF take out days_since_daily_new_confirmed_deaths_due_to_covid_19_rolling_7_day_average_right_aligned_reached_5  
rf_covid_fm_19 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_19)
max(vif(rf_covid_fm_19))
# from VIF take out daily_new_confirmed_deaths_due_to_covid_19_rolling_3_day_average_right_aligned  
rf_covid_fm_20 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_20)
max(vif(rf_covid_fm_20))
# from VIF take out daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned  
rf_covid_fm_21 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+containment_index+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_21)
max(vif(rf_covid_fm_21))
# from VIF take out containment_index 
rf_covid_fm_22 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_with_access_to_improved_sanitation_x+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_22)
max(vif(rf_covid_fm_22))
# from VIF take out population_with_access_to_improved_sanitation_x
rf_covid_fm_23 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_23)
max(vif(rf_covid_fm_23))
# from VIF take out days_since_the_total_confirmed_cases_of_covid_19_reached_100_with_population_5m
rf_covid_fm_24 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+days_since_5_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_24)
max(vif(rf_covid_fm_24))
# from VIF take out days_since_5_daily_new_confirmed_deaths_recorded
rf_covid_fm_25 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases+
                       days_since_30_daily_new_confirmed_cases_recorded+days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_25)
max(vif(rf_covid_fm_25))
# from VIF take out days_since_30_daily_new_confirmed_cases_recorded
rf_covid_fm_26 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases++days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_26)
max(vif(rf_covid_fm_26))
# from VIF take out days_since_daily_new_confirmed_cases_of_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_1
rf_covid_fm_27 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases++days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_27)
max(vif(rf_covid_fm_27))
# from VIF take out daily_new_confirmed_cases_of_covid_19_rolling_3_day_average_right_aligned
rf_covid_fm_28 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ stringency_index+
                       income_support+debt_relief+
                       population_without_access_to_improved_sanitation_x+
                       total_confirmed_deaths_due_to_covid_19+
                       daily_new_confirmed_cases_of_covid_19_per_million_people+
                       daily_new_confirmed_deaths_due_to_covid_19_per_million_people+
                       total_confirmed_cases_of_covid_19_per_million_people+
                       days_since_the_total_confirmed_deaths_of_covid_19_reached_5+
                       days_since_the_total_confirmed_cases_of_covid_19_per_million_people_reached_1+
                       case_fatality_rate_of_covid_19_only_observations_with_100_cases++days_since_50_daily_new_confirmed_cases_recorded+
                       days_since_10_daily_new_confirmed_deaths_recorded+
                       days_since_3_daily_new_confirmed_deaths_recorded+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_1+
                       days_since_daily_new_confirmed_deaths_due_to_covid_19_per_million_people_rolling_7_day_average_right_aligned_reached_0_01+
                       doubling_days_of_total_confirmed_cases_3_day_period+
                       doubling_days_of_total_confirmed_deaths_3_day_period+
                       weekly_case_growth+weekly_death_growth+biweekly_case_growth+
                       biweekly_death_growth+biweekly_deaths_per_million_people+
                       case_fatality_rate_of_covid_19_short_term,data = covid_data)
vif(rf_covid_fm_28)
max(vif(rf_covid_fm_28))
step(rf_covid_fm_28)
final_model_after_step <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
                               debt_relief + population_without_access_to_improved_sanitation_x + 
                               total_confirmed_deaths_due_to_covid_19 + daily_new_confirmed_cases_of_covid_19_per_million_people + 
                               total_confirmed_cases_of_covid_19_per_million_people + 
                               case_fatality_rate_of_covid_19_only_observations_with_100_cases + 
                               days_since_10_daily_new_confirmed_deaths_recorded + weekly_case_growth + 
                               weekly_death_growth + biweekly_deaths_per_million_people + 
                               case_fatality_rate_of_covid_19_short_term, data = covid_data)
summary(rf_covid_fm_28)