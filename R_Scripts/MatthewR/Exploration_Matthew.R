# Matthew Testing Beginning to Look at Data

library(readr)
library(ggplot2)
library(dplyr)
library(naniar)
library(car)
library(visdat)


# Read in the combined data-frame (maybe minus economic data)
initial_final_df <- read_csv("initial_final_df.csv")

# removed the continents (i.e. they are not countries)
df_minus_continents <-subset(initial_final_df, Entity!= "Africa" & Entity!= "Europe" & Entity!= "World" & Entity!= "South-East Asia" & Entity!= "Western Pacific")

# removed a blank column which was at the start
df_minus_continents <- df_minus_continents[-c(1)]

# wrote this new and now our 'for now' final dataframe to a csv file.

write.csv(df_minus_continents,"country_variable_data_cleared.csv", row.names = TRUE)

# This shows that there is quite alot of missing data.
# looks like all of the health data might be missing/got lost in the merge to the big dataframe.

missing_data_summary <- miss_var_summary(df_minus_continents)
missing_data_summary

# testing what happens when you omit all the NA's

omit_df <- na.omit(df_minus_continents)

# it's an empty dataframe, which means we have NA's in each row/column!




