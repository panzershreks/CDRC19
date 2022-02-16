# Time Series Attempt

library(readr)
library(tidyverse)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

owid_covid_data <- read_csv("Data_Dump/OWID_Data/owid-covid-data.csv")


subset_df <- data.frame(owid_covid_data$date,owid_covid_data$location, owid_covid_data$total_deaths, 
                        owid_covid_data$new_deaths, owid_covid_data$new_deaths_smoothed)


world_data <- subset_df %>% filter(owid_covid_data.location == 'World')
world_data <- subset(world_data, select = -c(owid_covid_data.location))
colnames(world_data) <- c("Date", "Cumulative_Deaths", "New_Deaths", "Smooth_New_Deaths")

cum_death_plot <- ggplot(data = world_data, aes(Date, Cumulative_Deaths)) + geom_bar(stat = "identity", fill = "aquamarine3") +
  labs(title = "Cumulative Deaths",
       x = "Date", y = "Cumulative Worldwide Covid-19 Deaths") + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels =  "%m-%y")

smooth_new_death_plot <- ggplot(data = world_data, aes(Date, Smooth_New_Deaths)) + geom_bar(stat = "identity", fill = "aquamarine3") +
  labs(title = "New Deaths",
       x = "Date", y = "New Worldwide Covid-19 Deaths") + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels =  "%m-%y") 

death_world <- grid.arrange(cum_death_plot, smooth_new_death_plot, top = textGrob("Worldwide Covid-19 Deaths", gp=gpar(col="red", fontface = "bold", fontsize = 15)))

# ggsave(death_world, filename = "R_Scripts//MatthewR//Report Graphs//deaths_world.png", height = 8, width = 14, units = "in")


# We now want to do the same for the UK


uk_data <- subset_df %>% filter(owid_covid_data.location == 'United Kingdom')
uk_data <- subset(uk_data, select = -c(owid_covid_data.location))
colnames(uk_data) <- c("Date", "Cumulative_Deaths", "New_Deaths", "Smooth_New_Deaths")

cum_death_plot <- ggplot(data = uk_data, aes(Date, Cumulative_Deaths)) + geom_bar(stat = "identity", fill = "aquamarine3") +
  labs(title = "Cumulative Deaths",
       x = "Date", y = "Cumulative UK Covid-19 Deaths") + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels =  "%m-%y")

smooth_new_death_plot <- ggplot(data = uk_data, aes(Date, Smooth_New_Deaths)) + geom_bar(stat = "identity", fill = "aquamarine3") +
  labs(title = "New Deaths",
       x = "Date", y = "New UK Covid-19 Deaths") + scale_y_continuous(labels = comma) + scale_x_date(date_breaks = "1 month", date_labels =  "%m-%y") 

death_uk <- grid.arrange(cum_death_plot, smooth_new_death_plot, top = textGrob("United Kingdom Covid-19 Deaths", gp=gpar(col="red", fontface = "bold", fontsize = 15)))

ggsave(death_uk, filename = "R_Scripts//MatthewR//Report Graphs//deaths_uk.png", height = 8, width = 14, units = "in")












