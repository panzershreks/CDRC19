library("ggplot2")
library("tidyverse")
library("readr")


World_Happiness_Report_2021_ <- read_csv("Working_Data/world_stats/World Happiness Report (2021).csv")

d2 <- World_Happiness_Report_2021_ %>% group_by(Entity) %>% slice_max(Year)
d2 <- subset(d2, select=-c(Year))
write.csv(d2,"happyness_combined.csv", row.names = TRUE)
