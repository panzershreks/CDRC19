# In this script we combine the dataframes together.

library(readr)

# We first read in the disease category data, which includes missing values.

clean_disease <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_disease.csv")
clean_disease <- clean_names(clean_disease)
clean_disease <- subset(clean_disease, select = -c(1,2))
clean_disease <- clean_disease[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                  145, 151, 156, 171,173,177,178, 186,193),]

# We then read in the food/water/world statistics category data, which includes missing values.

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -c(1,2))
clean_food_water <- clean_food_water[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                        145, 151, 156, 171,173,177,178, 186,193), ]

clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,2,3))
clean_world_stats <- clean_world_stats[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                          145, 151, 156, 171,173,177,178, 186,193),]

clean_combined_f_ws <- cbind(clean_food_water, clean_world_stats)
clean_combined_f_ws <- subset(clean_combined_f_ws, select = -c(1))


# We now want to combine these two dataframes into one.


combined_cats_with_missing <- cbind(clean_disease, clean_combined_f_ws)

# We now want to select our significant variables from this, to give us a dataframe with our significant variables,
# but with missing values.

# healthy diet cost percent cannot afford = 41
# age standardised diabetes prevalence male = 2
# cardiovascular diseases ihme 2017 = 16
# meningitis ihme 2017 = 24
# prevalence of obesity female who 2019 = 29

# Now we create a df with our significant variables

combined_cats_with_missing <- subset(combined_cats_with_missing, select = c(1,2,16,24,29,41))
# write.csv(combined_cats_with_missing, file = "sig_var_w_missing.csv", row.names = TRUE)

# We will now repeat this, but with our random forest imputed data

food_world_stats_complete <- read_csv("R_Scripts/MatthewR/Category Analysis (new)/CSV Files/food_world_stats_complete.csv")
food_world_stats_complete <- clean_names(food_world_stats_complete)
food_world_stats_complete <- subset(food_world_stats_complete, select = -c(1,2))

disease_data_complete <- read_csv("R_Scripts/MatthewR/Category Analysis (new)/CSV Files/disease_data_complete.csv")
disease_data_complete <- clean_names(disease_data_complete)
disease_data_complete <- subset(disease_data_complete, select = -c(1))

# Now we want to combine these two dataframes

combined_cats_complete <- cbind(disease_data_complete,food_world_stats_complete)

# Now we create a df with our significant variables

combined_cats_complete <- subset(combined_cats_complete, select = c(1,2,16,24,29,41))
# write.csv(combined_cats_complete, file = "sig_var_complete.csv", row.names = TRUE)






