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

# Read in the data

combined_disease <- read_csv("R_Scripts/MatthewR/Category Analysis/S1 Combined/working_disease.csv")
combined_disease <- clean_names(combined_disease)
combined_disease <- subset(combined_disease, select = -1)

combined_food <- read_csv("R_Scripts/MatthewR/Category Analysis/S1 Combined/working_food.csv")
combined_food <- clean_names(combined_food)
combined_food <- subset(combined_food, select = -1)

world_stats_1 <- read_csv("R_Scripts/MatthewR/Category Analysis/S1 Combined/world_stats_1.csv")
world_stats_1 <- clean_names(world_stats_1)
world_stats_1 <- subset(world_stats_1, select = -1)

# Now we want to take the significant variables we have identifed from each category.
# We note that we miss the data for the ten countries which we omitted after they couldn't
# be imputed in the disease data section, but since its only ten countries this is okay.


holding_1 <- merge(combined_disease,combined_food,by = "entity")

combined_df <- merge(holding_1, world_stats_1, by = "entity")

#write.csv(combined_df, file = "big_model_combined_df.csv", row.names = TRUE)

# 2 = response
# disease -> 17, 19 and 29
# world stats = last one = 47
# Food and Water = 40


significant_variables <- subset(combined_df, select = c(2, 17, 19, 29, 40, 47))
# write.csv(significant_variables, file = "significant_variables.csv", row.names = TRUE)

# Look at correlation:


Mcor <- significant_variables
cor1 <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

# ggsave(cor1, file="cor1.png", width = 7.5)
cor_data_frame <- round(cor(Mcor),2)
# write.csv(cor_data_frame,"combined_correlation.csv", row.names = TRUE)


full_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people.x ~ cardiovascular_diseases_ihme_2017 +
                   kidney_disease_ihme_2017 +	prevalence_of_obesity_male_who_2019 +	healthy_diet_cost_percent_cannot_afford	+ 
                   life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = significant_variables)
vif(full_model)

# We remove no variables as all VIF values are under 5.

step_full_model <- step(full_model)
summary(step_full_model)





























