rm(list=ls())
library(dplyr)
library(tidyverse)
library(purrr)

#Load Economic data CSV's into R DF's

temp <- dir("./../../Working_Data/economic", recursive=TRUE, full.names=TRUE, pattern="\\.csv$")
temp_names <- list.files("./../../Working_Data/economic")
k <- 1
for (i in temp){
  df <- read.csv(i)
  assign(temp_names[k], df)
  k <- k + 1
}
rm(i, k, df, temp, temp_names)

# removed DFs with overlapping or non-country data

rm(`GDP per capita indexed at 1950 - Maddison Project Data (2018).csv`,
   `GNP by country 2021.csv`,
   `Income Inequality - World Bank (2016).csv`,
   `Maddison Project Database 2020 (Bolt and van Zanden (2020)).csv`,
   `Wealth (total) by component for various country groupings - World Bank (2017).csv`,
   `World GDP in 2011 int $ – OWID based on World Bank + Maddison (2017).csv`)

# `Average monthly incomes or consumption by decile and quintile – PovcalNet (2019).csv`
# Processed to:
# avg_mthly_income_or_consump_by_dec_and_quint

avg_mthly_income_or_consump_by_dec_and_quint <- `Average monthly incomes or consumption by decile and quintile – PovcalNet (2019).csv`
rm(`Average monthly incomes or consumption by decile and quintile – PovcalNet (2019).csv`)
colnames(avg_mthly_income_or_consump_by_dec_and_quint)[colnames(avg_mthly_income_or_consump_by_dec_and_quint) == 'mean'] <- 'mean_mthly_income'
colnames(avg_mthly_income_or_consump_by_dec_and_quint)[colnames(avg_mthly_income_or_consump_by_dec_and_quint) == 'flag'] <- 'mthly_income_flag'
colnames(avg_mthly_income_or_consump_by_dec_and_quint)[colnames(avg_mthly_income_or_consump_by_dec_and_quint) == 'welfare'] <- 'income_welfare'
avg_mthly_income_or_consump_by_dec_and_quint <- avg_mthly_income_or_consump_by_dec_and_quint %>% 
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(pop, Year))

# `Country Income Classification - World Bank (2017).csv`
# Processed to:
# income_classification

income_classification <- `Country Income Classification - World Bank (2017).csv`
rm(`Country Income Classification - World Bank (2017).csv`)
income_classification <- income_classification %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# `economic-inequality-gini-index.csv`
# Processed to:
# gini_index

gini_index <- `economic-inequality-gini-index.csv`
rm(`economic-inequality-gini-index.csv`)
gini_index <- gini_index %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# `GDP growth from previous year, 2020 Q2 (Eurostat, OECD, National sources).csv`
# Processed to:
# gdp_growth

gdp_growth <- `GDP growth from previous year, 2020 Q2 (Eurostat, OECD, National sources).csv`
rm(`GDP growth from previous year, 2020 Q2 (Eurostat, OECD, National sources).csv`)
gdp_growth <- subset(gdp_growth, select=-c(Year))

# `GDP per capita PPP 2011 – WDI (2016).csv`
# Processed to:
# gdp_per_capita

gdp_per_capita <- `GDP per capita PPP 2011 – WDI (2016).csv`
rm(`GDP per capita PPP 2011 – WDI (2016).csv`)
gdp_per_capita <- gdp_per_capita %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# `Measures and indicators for Poverty - PovcalNet (World Bank) (2017).csv`
# Processed to:
# poverty_measures

poverty_measures <- `Measures and indicators for Poverty - PovcalNet (World Bank) (2017).csv`
rm(`Measures and indicators for Poverty - PovcalNet (World Bank) (2017).csv`)
poverty_measures <- poverty_measures %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year, Closing.the.poverty.gap.including.excluding.China..PovcalNet..World.Bank...2017..)) %>%
  subset(Entity != "World") %>%
  subset(Entity != "World no China")

# `National Poverty Lines - Jolliffe and Prydz (2016).csv`
# Processed to:
# poverty_lines

poverty_lines <- `National Poverty Lines - Jolliffe and Prydz (2016).csv`
rm(`National Poverty Lines - Jolliffe and Prydz (2016).csv`)
poverty_lines <- subset(poverty_lines, select=-c(Year))

# `OPHI Multidimensional Poverty Index - Alkire and Robles (2016).csv`
# Processed to:
# multidimentional_poverty_index

multidimentional_poverty_index <- `OPHI Multidimensional Poverty Index - Alkire and Robles (2016).csv`
rm(`OPHI Multidimensional Poverty Index - Alkire and Robles (2016).csv`)
multidimentional_poverty_index <- subset(multidimentional_poverty_index, select=-c(Year))

# `PolcalNet Global Poverty (2017).csv`
# Processed to:
# monthly_per_capita_expenditure

monthly_per_capita_expenditure <- `PolcalNet Global Poverty (2017).csv`
rm(`PolcalNet Global Poverty (2017).csv`)
monthly_per_capita_expenditure <- monthly_per_capita_expenditure %>% 
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# `Poverty rate (!50% of median) (LIS Key Figures, 2018).csv`
# Processed to:
# poverty_rate_50_perc_median

poverty_rate_50_perc_median <- `Poverty rate (!50% of median) (LIS Key Figures, 2018).csv`
rm(`Poverty rate (!50% of median) (LIS Key Figures, 2018).csv`)
poverty_rate_50_perc_median <- poverty_rate_50_perc_median %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# `Wealth per capita by component for various country groupings - World Bank (2017).csv`
# Processed to:
# regional_wealth_per_capita

regional_wealth_per_capita <- `Wealth per capita by component for various country groupings - World Bank (2017).csv`
rm(`Wealth per capita by component for various country groupings - World Bank (2017).csv`)
regional_wealth_per_capita <- regional_wealth_per_capita %>%
  group_by(Entity) %>%
  slice_max(Year) %>%
  subset(select=-c(Year))

# get list of OWID DF + processed DFs: df_list df_list_owid

df_names <- ls()

owid <- read.csv("./../../Data_Dump/OWID_Data/owid_latest.csv")
colnames(owid)[colnames(owid) == 'location'] <- 'Entity'
owid <- subset(owid, select=-c(date, X))

countries <- subset(owid, select=c(iso_code, continent, Entity))

df_list <- list()
df_list[[1]] <- countries
for (i in 2:(length(df_names)+1)){
  df_list[[i]] <- get(df_names[i-1])
}

df_list_owid <- list()
df_list_owid[[1]] <- owid
for (i in 2:(length(df_names)+1)){
  df_list_owid[[i]] <- get(df_names[i-1])
}

rm(i, df_names)

#list(x, y, z) %>% reduce(left_join, by = "i")

econ_df <- df_list %>% reduce(left_join, by="Entity")

write.csv(econ_df, file="../../economics_data.csv")

econ_owid_df <- df_list_owid %>% reduce(left_join, by="Entity")

write.csv(econ_owid_df, file="../../economics_plus_owid_data.csv")



