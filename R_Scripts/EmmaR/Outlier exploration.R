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
library(Amelia)
library(ggplot2)
library(dplyr)
library(xtable)

subset_of_total <- read_csv("GLM Take 2/Combined Model/subset_of_total.csv")
subset_of_total$index <- 1:nrow(subset_of_total)

missing_table <- miss_var_summary(subset_of_total, sort_miss = TRUE)
missing_table <- data.frame(missing_table)
View(missing_table)

# Missing data table by entities 

miss_table_entity <- subset_of_total %>%
  group_by(index) %>%
  miss_case_summary(order = TRUE) %>%
  order(miss_table_entity$pct_miss, decreasing = TRUE)

sum(is.na(subset_of_total[128, ])) / length(subset_of_total[128, ])



entity_and_response <- read_csv("GLM Take 2/Split Into Categories Initial Work/entity_and_response.csv")
View(entity_and_response)

subset_of_total$index <- 1:nrow(subset_of_total)

View(subset_of_total[c(9, 69,84), c(1, 4, 5, 7, 8, 16, 24, 27)])
View(subset_of_total[134, c(1, 4, 5, 7, 8, 16, 24, 27)])
View(subset_of_total[34, c(1, 4, 5, 7, 8, 16, 24, 27)])


View(entity_and_response)

# United States is 159 

View(subset_of_total[159, ])

View(subset_of_total[, c(1,27)])
View(subset_of_total[,c(1, 24)])
View(subset_of_total[,c(1, 8)])
View(subset_of_total[,c(1, 7)])
View(subset_of_total[,c(1, 5)])

owid_covid_data <- read_csv("Data_Dump/OWID_Data/owid-covid-data.csv")

subset_df <- data.frame(owid_covid_data$date,owid_covid_data$location, owid_covid_data$total_deaths, 
                        owid_covid_data$new_deaths, owid_covid_data$new_deaths_smoothed)


View(subset_of_total[c(9, 159), c(1, 4, 5, 7, 8, 16, 24, 27)])
View(subset_of_total[c(9, 159), c(1, 4, 5, 7, 8, 16, 24, 27)])

combined_all_missing <- read_csv("GLM Take 2/Combined Model/combined_all_missing.csv")
combined_all_missing$index <- 1:nrow(subset_of_total)
combined_all_missing$entity <- entity_and_response[,1]
View(combined_all_missing[,c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

# US and UK
View(combined_all_missing[c(159, 158),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

# US and Mexico
View(combined_all_missing[c(159, 100),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

# US and Brazil 
View(combined_all_missing[c(159, 23),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

View(combined_all_missing[c(110, 140),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 32),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 113),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 63),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 39),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 158),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[c(110, 158),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])
View(combined_all_missing[,c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

View(combined_all_missing[9,c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])


View(combined_all_missing[c(128, 120, 17),c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])

View(combined_all_missing[,c(116, 117, 2, 18, 20, 45, 48, 60, 79, 87) ])


source("LM Data and Analysis/list_of_new_countries.R")
# code to add country text to the plot,
par(mfrow=c(1,1))
plot(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people) + 
  text(fitted(step_drop_vif), full_df$total_confirmed_deaths_due_to_covid_19_per_million_people, 
       row.names(list_of_countries), cex=0.6, pos=4, col="red")

china_data <- subset_df %>% filter(owid_covid_data.location == 'China')
china_data <- subset(china_data, select = -c(owid_covid_data.location))
colnames(china_data) <- c("Date", "Cumulative_Deaths", "New_Deaths", "Smooth_New_Deaths")

china_plot <- ggplot(data = china_data, aes(Date, Smooth_New_Deaths)) + geom_bar(stat = "identity", fill = "aquamarine3") +
  labs(title = "China Covid-19 Deaths",
       x = "Date", y = "New Daily Covid-19 Deaths (Smoothed)") + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels =  "%m-%y")

china_plot

