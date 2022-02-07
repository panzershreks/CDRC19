# practicing with GLM

library(readr)
clean_world_stats <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_world_stats.csv")
clean_world_stats <- clean_names(clean_world_stats)
clean_world_stats <- subset(clean_world_stats, select = -c(1,2))
clean_world_stats <- na.omit(clean_world_stats)

# We are now going to use the GLM function

fit_test <- glm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ 
      life_satisfaction_in_cantril_ladder_world_happiness_report_2019, data = clean_world_stats,
    family = inverse.gaussian)

par(mfrow = c(2, 2))
plot(fit_test)




