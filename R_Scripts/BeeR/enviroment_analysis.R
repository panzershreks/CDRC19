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

clean_enviroment <- read.csv("~/Documents/University/YEAR 4/ALL YEAR/MATHEMATICAL PROJECT/ANALYSIS/CDRC19/Combined DataFrame Work/CSV Files/Clean/clean_enviroment.csv")
clean_enviroment <- clean_names(clean_enviroment)
clean_enviroment <- subset(clean_enviroment, select = -1)
# We look at the missing data and export the missing data table to a CSV for making into latex_

missing_table <- miss_var_summary(clean_enviroment, sort_miss = TRUE)
vis_miss(clean_enviroment)
missing_table

# write.csv(missing_table, file = "enviroment_missing.csv", row_names = TRUE)
# We now want to do imputation

#set_seed(100)
#enviroment_imputation <- mice(data = clean_enviroment, m = 5, method = c("cart"), maxit = 100)
# summary(enviroment_imputation)
# enviroment_imputation$loggedEvents
enviroment_1 <- complete(enviroment_imputation, 1)
enviroment_2 <- complete(enviroment_imputation, 2)
enviroment_3 <- complete(enviroment_imputation, 3)
enviroment_4 <- complete(enviroment_imputation, 4)
enviroment_5 <- complete(enviroment_imputation, 5)

working_enviroment <- enviroment_1
# write.csv(working_enviroment, file = "working_enviroment.csv", row_names = TRUE)

miss_var_summary(enviroment_1)
nrow(enviroment_1)
# now do correlation between values
# then VIF
# then model_

Mcor <- working_enviroment[,2:51]

enviroment_cor_matrix <- vis_cor(Mcor) + theme(axis_text_x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

vis_cor(Mcor) + theme(axis_text_x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

cor_data_frame <- round(cor(Mcor),2)
# write.csv(cor_data_frame,"enviroment_correlation.csv", row_names = TRUE)
# saving imputations
#save(enviroment_imputation, file = "enviroment_imputation.Rdata")
load(file = "enviroment_imputation.Rdata")
# Now you want to use VIF - we will remove the largest valued variable each time
# until we have that all the values are under five_
full_model <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
                 + indoor_15_to_19_years
                 + indoor_20_to_24_years 
                 + indoor_25_to_29_years 
                 + indoor_30_to_34_years 
                 + indoor_35_to_39_years 
                 + indoor_40_to_44_years 
                 + indoor_45_to_49_years  
                 + indoor_5_to_9_years 
                 + indoor_50_to_54_years 
                 + indoor_55_to_59_years 
                 + indoor_60_to_64_years 
                 + indoor_65_to_69_years  
                 + indoor_70_to_74_years 
                 + indoor_75_to_79_years  
                 + indoor_80_years
                 + indoor_under_5s 
                 + outdoor_10_to_14_years 
                 + outdoor_15_to_19_years  
                 + outdoor_20_to_24_years 
                 + outdoor_25_to_29_years 
                 + outdoor_30_to_34_years 
                 + outdoor_35_to_39_years 
                 + outdoor_40_to_44_years 
                 + outdoor_45_to_49_years  
                 + outdoor_5_to_9_years
                 + outdoor_50_to_54_years 
                 + outdoor_55_to_59_years 
                 + outdoor_60_to_64_years 
                 + outdoor_65_to_69_years  
                 + outdoor_70_to_74_years 
                 + outdoor_75_to_79_years  
                 + outdoor_80_years 
                 + outdoor_under_5s 
                 + excess_mortality_from_air_pollution_all_sources
                 + excess_mortality_from_fossil_fuels 
                 + excess_mortality_from_all_anthropogenic_pollution 
                 + total_years_life_lost_from_air_pollution_all_sources
                 + total_years_life_lost_from_fossil_fuels 
                 + total_years_life_lost_from_all_anthropogenic_pollution 
                 + death_rates_from_all_air_pollution_per_100_000 
                 + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
                 + death_rates_from_all_anthropogenic_air_pollution_per_100_000
                 + yll_rates_from_all_air_pollution_per_100_000 
                 + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
                 + yll_rates_from_anthropogenic_air_pollution_per_100_000 
                 + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
                 + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
                 + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(full_model)
# remove outdoor_50_to_54_years 
vif_1 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_35_to_39_years 
            + indoor_40_to_44_years 
            + indoor_45_to_49_years  
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_65_to_69_years  
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_30_to_34_years 
            + outdoor_35_to_39_years 
            + outdoor_40_to_44_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_1)
# remove indoor_65_to_69_years  
vif_2 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_35_to_39_years 
            + indoor_40_to_44_years 
            + indoor_45_to_49_years  
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_30_to_34_years 
            + outdoor_35_to_39_years 
            + outdoor_40_to_44_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_2)
# remove indoor_35_to_39_years
vif_3 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_45_to_49_years  
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_30_to_34_years 
            + outdoor_35_to_39_years 
            + outdoor_40_to_44_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_3)
# remove indoor_45_to_49_years 
vif_4 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_30_to_34_years 
            + outdoor_35_to_39_years 
            + outdoor_40_to_44_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_4)
# remove outdoor_40_to_44_years
vif_5 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_30_to_34_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_5)
# remove outdoor_30_to_34_years
vif_6 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_50_to_54_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_6)
# remove indoor_50_to_54_years
vif_7 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_60_to_64_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_7)
# remove outdoor_60_to_64_years
vif_8 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_75_to_79_years  
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_8)
# remove indoor_75_to_79_years
vif_9 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_55_to_59_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_9)
# remove indoor_55_to_59_years
vif_10 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
            + indoor_15_to_19_years
            + indoor_20_to_24_years 
            + indoor_25_to_29_years 
            + indoor_30_to_34_years 
            + indoor_40_to_44_years 
            + indoor_5_to_9_years 
            + indoor_60_to_64_years 
            + indoor_70_to_74_years 
            + indoor_80_years
            + indoor_under_5s 
            + outdoor_10_to_14_years 
            + outdoor_15_to_19_years  
            + outdoor_20_to_24_years 
            + outdoor_25_to_29_years 
            + outdoor_35_to_39_years 
            + outdoor_45_to_49_years  
            + outdoor_5_to_9_years
            + outdoor_55_to_59_years 
            + outdoor_65_to_69_years  
            + outdoor_70_to_74_years 
            + outdoor_75_to_79_years  
            + outdoor_80_years 
            + outdoor_under_5s 
            + excess_mortality_from_air_pollution_all_sources
            + excess_mortality_from_fossil_fuels 
            + excess_mortality_from_all_anthropogenic_pollution 
            + total_years_life_lost_from_air_pollution_all_sources
            + total_years_life_lost_from_fossil_fuels 
            + total_years_life_lost_from_all_anthropogenic_pollution 
            + death_rates_from_all_air_pollution_per_100_000 
            + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
            + death_rates_from_all_anthropogenic_air_pollution_per_100_000
            + yll_rates_from_all_air_pollution_per_100_000 
            + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
            + yll_rates_from_anthropogenic_air_pollution_per_100_000 
            + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
            + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
            + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_10)
# remove excess_mortality_from_air_pollution_all_sources
vif_11 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_15_to_19_years
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_45_to_49_years  
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_65_to_69_years  
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + excess_mortality_from_fossil_fuels 
             + excess_mortality_from_all_anthropogenic_pollution 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_11)
# remove excess_mortality_from_fossil_fuels
vif_12 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_15_to_19_years
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_45_to_49_years  
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_65_to_69_years  
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + excess_mortality_from_all_anthropogenic_pollution 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_12)
# remove outdoor_45_to_49_years
vif_13 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_15_to_19_years
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_65_to_69_years  
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + excess_mortality_from_all_anthropogenic_pollution 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_13)
# remove excess_mortality_from_all_anthropogenic_pollution
vif_14 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_15_to_19_years
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_65_to_69_years  
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_14)
# remove indoor_15_to_19_years
vif_15 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_65_to_69_years  
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_15)
# remove outdoor_65_to_69_years
vif_16 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_35_to_39_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_16)
# remove outdoor_35_to_39_years
vif_17 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_70_to_74_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_17)
# remove outdoor_70_to_74_years
vif_18 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_5_to_9_years
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_18)
# remove outdoor_5_to_9_years
vif_19 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_25_to_29_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_19)
# remove outdoor_25_to_29_years
vif_20 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_70_to_74_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_20)
# remove indoor_70_to_74_years
vif_21 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_40_to_44_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_21)
# remove indoor_40_to_44_years
vif_22 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_fossil_fuels 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_22)
# remove total_years_life_lost_from_fossil_fuels
vif_23 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_air_pollution_all_sources
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_23)
# remove total_years_life_lost_from_air_pollution_all_sources
vif_24 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_25_to_29_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_24)
# remove indoor_25_to_29_years 
vif_25 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_75_to_79_years  
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_25)
# remove outdoor_75_to_79_years
vif_26 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_60_to_64_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_26)
# remove indoor_60_to_64_years
vif_27 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~ indoor_10_to_14_years 
             + indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_27)
# remove indoor_10_to_14_years
vif_28 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
             indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + total_years_life_lost_from_all_anthropogenic_pollution 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_28)
# remove total_years_life_lost_from_all_anthropogenic_pollution 
vif_29 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_15_to_19_years  
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_29)
# remove outdoor_15_to_19_years
vif_30 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_20_to_24_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_30)
# remove outdoor_20_to_24_years
vif_31 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + outdoor_under_5s 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_31)
# remove outdoor_under_5s
vif_32 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_air_pollution_from_fossil_fuels_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_32)
# remove death_rates_from_air_pollution_from_fossil_fuels_per_100_000
vif_33 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_55_to_59_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_33)
# remove outdoor_55_to_59_years
vif_34 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_80_years
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_34)
# remove indoor_80_years
vif_35 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + death_rates_from_all_anthropogenic_air_pollution_per_100_000
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_35)
# remove death_rates_from_all_anthropogenic_air_pollution_per_100_000
vif_36 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + indoor_under_5s 
             + outdoor_10_to_14_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_36)
# remove indoor_under_5s
vif_37 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_10_to_14_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_37)
# remove yll_rates_from_air_pollution_from_fossil_fuels_per_100_000
vif_38 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_10_to_14_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_38)
# remove outdoor_10_to_14_years
vif_39 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_20_to_24_years 
             + indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_39)
# remove indoor_20_to_24_years
vif_40 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_40)
# remove deaths_from_fossil_pollution_as_a_share_of_total_air_pollution_deaths
vif_41 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_anthropogenic_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_41)
# remove yll_rates_from_anthropogenic_air_pollution_per_100_000
vif_42 <- lm(total_confirmed_deaths_due_to_covid_19_per_million_people ~  
               indoor_30_to_34_years 
             + indoor_5_to_9_years 
             + outdoor_80_years 
             + death_rates_from_all_air_pollution_per_100_000 
             + yll_rates_from_all_air_pollution_per_100_000 
             + deaths_from_fossil_pollution_as_a_share_of_total_anthropogenic_air_pollution_deaths 
             + deaths_from_anthropogenic_pollution_as_a_share_of_total_air_pollution_deaths, data = working_enviroment)
vif(vif_42)
# We now have all the values under five, so we can say all these variables deserve to be in the model.
# Now we will ues the step function to find significant variables.

# step_enviroment <- step(vif_42, direction = c("both")) # , k=log(nrow(vif_42))
step_enviroment <- step(vif_42)
summary(step_enviroment)
plot(step_enviroment)

