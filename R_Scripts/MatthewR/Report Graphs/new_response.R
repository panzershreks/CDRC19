# Here, we analyse the response variable.

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
library('missForest')
library(readr)
library(scales)


clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,4))

summary(clean_world_stats$total_confirmed_deaths_due_to_covid_19_per_million_people)
plot(clean_world_stats$total_confirmed_deaths_due_to_covid_19_per_million_people)
clean_world_stats$total_confirmed_deaths_due_to_covid_19_per_million_people



cp_1 <- ggplot(data = clean_world_stats[0:50,], aes(x = entity , y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point()  + xlab(NULL) +
  ylab(NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

cp_2 <- ggplot(data = clean_world_stats[51:100,], aes(x = entity , y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL)  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

cp_3 <- ggplot(data = clean_world_stats[101:150,], aes(x = entity , y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))

cp_4 <- ggplot(data = clean_world_stats[151:193,], aes(x = entity , y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) +
  ylab(NULL)  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  scale_x_discrete(label=function(x) abbreviate(x, minlength=7))




covid_summary_plot <- grid.arrange(cp_1, cp_2, cp_3, cp_4, left = "Total Confirmed Deaths due to Covid-19 per Million People", 
                                   top = textGrob("Covid-19 Death Data", gp=gpar(col="black", fontface = "bold", fontsize = 15)))
 

#ggsave(covid_summary_plot, filename = "R_Scripts//MatthewR//Report Graphs//death_summary.png", height = 8, width = 14, units = "in")

death_dist <- ggplot(clean_world_stats, aes(x=total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_histogram(fill="aquamarine3", color="black") + 
  geom_vline(aes(xintercept=mean(total_confirmed_deaths_due_to_covid_19_per_million_people, na.rm = TRUE)), color="blue", linetype="dashed", size=1) + 
  ggtitle("Covid-19 Death Rates Distribution") + xlab("Total Confirmed Covid-19 Deaths per Million") + ylab("Count")

ggsave(death_dist, filename = "R_Scripts//MatthewR//Report Graphs//death_dist.png", height = 8, width = 14, units = "in")
