# Getting the data

library(readr)
library("janitor")

clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)


# Disease Columns - 144:171
# Food Columns - 221:237
# World Satisfaction - 350

disease_all_missing <- subset(clean_fully_merged, select = c(144:171))
food_all_missing <- subset(clean_fully_merged, select = c(221:237))
world_stats_all_missing <- subset(clean_fully_merged, select = c(350))

# Now we do this for the response variable.
# The repsonse variable is # 57

response_variable <- clean_fully_merged[,57]


economic_all_missing <- subset(clean_fully_merged, select = c(238:273))
names(economic_all_missing)[names(economic_all_missing) == 'mean'] <- 'mean_monthly_income'
economic_all_missing$gdp_growth_per_capita_from_previous_year_2020_q2 <- with(economic_all_missing, gdp_growth_from_previous_year_2020_q2 / pop2021)
to_drop <- c("pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
             "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
             "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
             "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y",
             "gdp_per_capita_ppp_2011_wdi_2016", "gdp", "gdp_growth_from_previous_year_2020_q2",
             "pop2021", "gini_coefficient_world_bank_2016", 
             "median_monthly_per_capita_expenditure_in_2011_int_povcal_net_2017",
             "percentage_contribution_of_deprivations_in_education_to_overall_poverty_alkire_and_robles_2016",
             "percentage_contribution_of_deprivations_in_health_to_overall_poverty_alkire_and_robles_2016",
             "percentage_contribution_of_deprivations_in_living_standards_to_overall_poverty_alkire_and_robles_2016")

economic_all_missing = economic_all_missing[,!(names(economic_all_missing)%in% to_drop)]


# write.csv(economic_all_missing,"GLM Data and Analysis//Category CSV//economic_all_missing.csv", row.names = TRUE)
# write.csv(disease_all_missing,"GLM Data and Analysis//Category CSV//disease_data_complete.csv", row.names = TRUE)
# write.csv(food_all_missing,"GLM Data and Analysis//Category CSV//food_all_missing.csv", row.names = TRUE)
# write.csv(world_stats_all_missing,"GLM Data and Analysis//Category CSV//world_stats_all_missing.csv", row.names = TRUE)
# write.csv(response_variable,"GLM Data and Analysis//Category CSV//response_variable.csv", row.names = TRUE)

# We now combine them all into one dataframe

# we need to read the other dataframes in now:

covid_all_missing <- read_csv("GLM Data and Analysis/Category CSV/covid_all_missing.csv")
covid_all_missing <- clean_names(covid_all_missing)
covid_all_missing <- subset(covid_all_missing, select = -1)

healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")
healthcare_all_mis <- clean_names(healthcare_all_mis)
healthcare_all_mis <- subset(healthcare_all_mis, select = -c(1,2,3))

demographic_all_missing <- read_csv("GLM Data and Analysis/Category CSV/demographic_all_missing.csv")
demographic_all_missing <- clean_names(demographic_all_missing)


combined_all_missing <- cbind(response_variable, food_all_missing, 
                              disease_all_missing, world_stats_all_missing, 
                              economic_all_missing, covid_all_missing,
                              healthcare_all_mis, demographic_all_missing)


# write.csv(combined_all_missing,"GLM Take 2/Combined Model/combined_all_missing.csv", row.names = TRUE)





