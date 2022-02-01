# We install our packages

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
library(missForest)

# We read in the data and get it into a good form to use MICE imputation by cleaning the titles
# and removing the column the CSV file has added.

clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -1)

# We now remove the countries which have no response variable value.

clean_world_stats <- clean_world_stats[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                  145, 151, 156, 171,173,177,178, 186,193),]

# Summary Statistics

summary(clean_world_stats)

# Summary Statistics showing relationship - this removes the missing values.

summary_w_missing <- ggplot(data = clean_world_stats, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() +
  ggtitle("Life Satisfaction Indicator and Covid Death Rates") + labs(x="Life Satisfaction Indicator", y = "Total Confirmed Deaths due to Covid-19 per Million People") + 
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=12))

# ggsave(summary_w_missing, file="w_s_summary_stat_w_missing.png", height = 5, width=12)


# We look at the missing data, but we first rename the columns to make the output better.

clean_world_stats_new_name <- clean_world_stats
colnames(clean_world_stats_new_name) <- c("Country", "Total Confirmed Deaths per Million", "Life Satisfaction Indicator")
missing_data_plot <- vis_miss(clean_world_stats_new_name, sort_miss = TRUE) +theme(axis.text.x = element_text(angle = 90))
miss_var_summary(clean_world_stats)
# missing_data_plot

# ggsave(missing_data_plot, file="w_s_missing_data.png")


# We now want to impute the missing data, we set seed to make results reproducable.

# We first use the MICE package, with the PMM method.

set.seed(100)
world_stats_imputation <- mice(data = clean_world_stats, m = 5, method = c("pmm"), maxit = 100)

world_stats_1 <- complete(world_stats_imputation, 1)
world_stats_2 <- complete(world_stats_imputation, 2)
world_stats_3 <- complete(world_stats_imputation, 3)
world_stats_4 <- complete(world_stats_imputation, 4)
world_stats_5 <- complete(world_stats_imputation, 5)

#write.csv(world_stats_1, file = "world_stats_1.csv", row.names = TRUE)
#write.csv(world_stats_2, file = "world_stats_2.csv", row.names = TRUE)
#write.csv(world_stats_3, file = "world_stats_3.csv", row.names = TRUE)
#write.csv(world_stats_4, file = "world_stats_4.csv", row.names = TRUE)
#write.csv(world_stats_5, file = "world_stats_5.csv", row.names = TRUE)


# We can now do something slightly different to the other data categories as we only have one significant variable.
# We can pool together all the results as follows:

world_stats_model <- with(world_stats_imputation, lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ life_satisfaction_in_cantril_ladder_world_happiness_report_2019))

summary(pool(world_stats_model))


# The following code allows us to visualise the imputed data and how each dataset differs.

ws_mice_1 <- ggplot(data = world_stats_1, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) + ylab(NULL) +
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=5))

ws_mice_2 <- ggplot(data = world_stats_2, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) + ylab(NULL) + 
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=5))

ws_mice_3 <- ggplot(data = world_stats_3, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) + ylab(NULL) + 
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=5))

ws_mice_4 <- ggplot(data = world_stats_4, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) + ylab(NULL) + 
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=5))

ws_mice_5 <- ggplot(data = world_stats_5, aes(x = life_satisfaction_in_cantril_ladder_world_happiness_report_2019, y = total_confirmed_deaths_due_to_covid_19_per_million_people)) + 
  geom_point() + xlab(NULL) + ylab(NULL) +
  geom_smooth(method = "lm", se = FALSE) + theme(text = element_text(size=5))

ws_iumputed_summary <- grid.arrange(ws_mice_1, ws_mice_2, ws_mice_3, ws_mice_4, ws_mice_5, nrow = 3,
                                    left = "Total Confirmed Deaths due to Covid-19 per Million People",
                                    top = textGrob("Life Satisfaction and Covid-19 Death Rates (Imputed)", gp=gpar(col="red", fontface = "bold", fontsize = 15)),
                                    bottom = "Life Satisfaction Indicator")


# ggsave(ws_iumputed_summary, file="w_s_imputed_sum.png")
















































































