library("ggplot2")
library("tidyverse")
library("readr")

Affordability_of_diets_SOFI_2021_ <- read_csv("Working_Data/food_and_water/Affordability of diets (SOFI, 2021).csv")
Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI <- read_csv("Working_Data/food_and_water/Number of people with and without access to improved sanitation - OWID based on WDI.csv")

d1 <- subset(Affordability_of_diets_SOFI_2021_, select=-c(Year))
d2 <- Number_of_people_with_and_without_access_to_improved_sanitation_OWID_based_on_WDI %>% group_by(Entity) %>% slice_max(Year)
d2 <- subset(d2, select=-c(Year))

food_water_df <- merge(d1, d2, by = "Entity", all = TRUE)
write.csv(food_water_df,"food_water_data.csv", row.names = TRUE)
