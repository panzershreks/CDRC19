library("ggplot2")
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library("mice")
library(UpSetR)
library("janitor")

clean_world_stats
vis_miss(clean_world_stats, sort_miss = TRUE)
miss_var_summary(clean_world_stats)

clean_world_stats <- clean_names(clean_world_stats)
world_stats_imputation <- mice(data = clean_world_stats, m = 5, method = c("cart"), maxit = 100)

world_stats_1 <- complete(world_stats_imputation, 1)
world_stats_2 <- complete(world_stats_imputation, 2)
world_stats_3 <- complete(world_stats_imputation, 3)
world_stats_4 <- complete(world_stats_imputation, 4)
world_stats_5 <- complete(world_stats_imputation, 5)


world_stats_model <- with(world_stats_imputation, lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ life_satisfaction_in_cantril_ladder_world_happiness_report_2019))

summary(world_stats_model)
summary(pool(world_stats_model))

# Let's take the first imputed dataframe and see what happens when we do a LM only on it.

ws_1_fit <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = world_stats_1)

summary(ws_1_fit)

ggplot(data = world_stats_1, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + geom_point() +
  ggtitle("World Statistics Category") + labs(x="Life Satisfaction Indicator", y = "Total Confirmed Deaths due to Covid-19 per Million People") + geom_smooth(method = "lm")

