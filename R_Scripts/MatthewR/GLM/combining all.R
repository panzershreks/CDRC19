# Getting the data

library(readr)

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

#write.csv(disease_all_missing,"disease_data_complete.csv", row.names = TRUE)
#write.csv(food_all_missing,"food_all_missing.csv", row.names = TRUE)
#write.csv(world_stats_all_missing,"world_stats_all_missing.csv", row.names = TRUE)

economic_all_missing <- subset(clean_fully_merged, select = c(238:273))
to_drop <-c("pop", "d1avgincome", "d2avgincome", "d3avgincome", "d4avgincome",
  "d5avgincome", "d6avgincome", "d7avgincome", "d8avgincome",
  "d9avgincome", "d10avgincome", "q1avgincome", "q2avgincome",
  "q3avgincome", "q4avgincome", "q5avgincome", "flag", "population_y",
  "gdp_per_capita_ppp_2011_wdi_2016", "gdp", "gdp_growth_from_previous_year_2020_q2",
  "pop2021")

economic_all_missing = economic_all_missing[,!(names(economic_all_missing)%in% to_drop)]
#write.csv(economic_all_missing,"economic_all_missing.csv", row.names = TRUE)



