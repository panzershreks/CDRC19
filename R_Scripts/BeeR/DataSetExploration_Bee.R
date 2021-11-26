temp <- dir("./../../Working_Data/covid", recursive=TRUE, full.names=TRUE, pattern="\\.csv$")
temp_names <- list.files("./../../Working_Data/covid")
k <- 1
for (i in temp){
  df <- read.csv(i)
  assign(temp_names[k], df)
  k <- k + 1
}
rm(i, k, df, temp, temp_names)

owid_latest <- read_csv("Data_Dump/OWID_Data/owid_latest.csv")
owid_latest_df <- data.frame(owid_latest)
View(owid_latest)
df1 <- owid_latest_df
setNames(df1, replace(names(df1), names(df1) == 'location', 'Entity'))
df2 <- COVID_Government_Response_OxBSG_df
data <- merge(by.x = df1, by.y = df2, by = "Entity", all.x = TRUE)
View(data)
View(df1)
View(df2)

df_list <- list(COVID_19_Tests_df, COVID_19_Tests_per_million_people_df, COVID_19_Variants_df, COVID_2019_ECDC_2020_df, 
                COVID_2019_Hospital_ICU_df, COVID_Government_Response_OxBSG_df, COVID_testing_time_series_data_df,
                State_of_Vaccine_Confidence_Larson_et_al_2016_df)
Reduce(function(x, y) merge(x, y, by = "Entity", all=TRUE), df_list)
View(df_list)
df_list %>% reduce(full_join, by='Entity')
df4 <- sqldf("SELECT location 
              FROM df1
              LEFT JOIN df2 USING(Entity)")


data_new <- join(df1, df2, 
                 type = "left")
View(data_new)

data_secondtry <- merge(x = COVID_19_Tests_df, y = COVID_19_Tests_per_million_people_df , by = "Entity", all.x = TRUE)
View(data_secondtry)

Combined_data <- c(COVID_19_Tests_per_million_people_df, COVID_19_Variants_df, COVID_2019_ECDC_2020_df)
View(Combined_data)

data_tertiary <- merge(x = COVID_19_Tests_df, y = Combined_data , by = "Entity", all.x = TRUE)
View(data_tertiary)


data(package = "dplyr")
load("~/GitHub/CDRC19/Working_Data.rda")

library(readr)
install.packages("sqldf")
library(sqldf)
install.packages("plyr")
library(plyr)
library(tidyverse)

# COVID DATA SET IMPORT AND COVERT TO DATA FRAME
COVID_Government_Response_OxBSG_ <- read_csv("Working_Data/covid/COVID Government Response (OxBSG).csv")
COVID_Government_Response_OxBSG_df <- data.frame(COVID_Government_Response_OxBSG_) %>% group_by(Entity) %>% slice_max(Year) 
COVID_Government_Response_OxBSG_df_NO_YEAR <- subset(COVID_Government_Response_OxBSG_df, select= -c(Year))

COVID_testing_time_series_data <- read_csv("Working_Data/covid/COVID testing time series data.csv")
COVID_testing_time_series_data_df <- data.frame(COVID_testing_time_series_data) %>% group_by(Entity) %>% slice_max(Year) 
COVID_testing_time_series_data_df_NO_YEAR <- subset(COVID_testing_time_series_data_df, select= -c(Year))

COVID_19_Variants <- read_csv("Working_Data/covid/COVID-19 - Variants.csv")
COVID_19_Variants_df <- data.frame(COVID_19_Variants) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Variants_df_NO_YEAR <- subset(COVID_19_Variants_df, select= -c(Year))

COVID_19_Tests_per_million_people <- read_csv("Working_Data/covid/COVID-19 Tests per million people.csv")
COVID_19_Tests_per_million_people_df <- data.frame(COVID_19_Tests_per_million_people) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Tests_per_million_people_df_NO_YEAR <- subset(COVID_19_Tests_per_million_people_df, select= -c(Year))

COVID_19_Tests <- read_csv("Working_Data/covid/COVID-19 Tests.csv")
COVID_19_Tests_df <- data.frame(COVID_19_Tests) %>% group_by(Entity) %>% slice_max(Year) 
COVID_19_Tests_df_NO_YEAR <- subset(COVID_19_Tests_df, select= -c(Year))

COVID_2019_ECDC_2020_ <- read_csv("Working_Data/covid/COVID-2019 - ECDC (2020).csv")
COVID_2019_ECDC_2020_df <- data.frame(COVID_2019_ECDC_2020_) %>% group_by(Entity) %>% slice_max(Year) 
COVID_2019_ECDC_2020_df_NO_YEAR <- subset(COVID_2019_ECDC_2020_df, select= -c(Year))

COVID_2019_Hospital_ICU <- read_csv("Working_Data/covid/COVID-2019 - Hospital & ICU.csv")
COVID_2019_Hospital_ICU_df <- data.frame(COVID_2019_Hospital_ICU) %>% group_by(Entity) %>% slice_max(Year) 
COVID_2019_Hospital_ICU_df_NO_YEAR <- subset(COVID_2019_Hospital_ICU_df, select= -c(Year))

State_of_Vaccine_Confidence_Larson_et_al_2016_ <- read_csv("Working_Data/covid/State of Vaccine Confidence - Larson et al (2016).csv")
State_of_Vaccine_Confidence_Larson_et_al_2016_df <- data.frame(State_of_Vaccine_Confidence_Larson_et_al_2016_) %>% group_by(Entity) %>% slice_max(Year) 
State_of_Vaccine_Confidence_Larson_et_al_2016_df_NO_YEAR <- subset(State_of_Vaccine_Confidence_Larson_et_al_2016_df, select= -c(Year))

# COVID DATA FRAMES MERGE TO SINGLE DATA FRAME

merge_1.1 <- merge(x = COVID_19_Tests_df_NO_YEAR, 
                   y = COVID_19_Tests_per_million_people_df_NO_YEAR , by = "Entity", all.x = TRUE)
  
merge_1.2 <- merge(x = COVID_19_Variants_df_NO_YEAR, 
                   y = COVID_2019_ECDC_2020_df_NO_YEAR , by = "Entity", all.x = TRUE)
  
merge_1.3 <- merge(x = COVID_2019_Hospital_ICU_df_NO_YEAR, 
                   y = COVID_Government_Response_OxBSG_df_NO_YEAR , by = "Entity", all.x = TRUE)

merge_1.4 <- merge(x = COVID_testing_time_series_data_df_NO_YEAR, 
                   y = State_of_Vaccine_Confidence_Larson_et_al_2016_df_NO_YEAR , by = "Entity", all.x = TRUE)

merge_2.1 <- merge(x = merge_1.1, 
                   y = merge_1.2 , by = "Entity", all.x = TRUE)

merge_2.2 <- merge(x = merge_1.3, 
                   y = merge_1.4 , by = "Entity", all.x = TRUE)

Covid_combined_data <- merge(x = merge_2.1, 
                            y = merge_2.2 , by = "Entity", all.x = TRUE)

View(Covid_combined_data)

# CREATE CSV FILE FOR DATA FRAME FOR COVID

write.csv(Covid_combined_data, "Covid_combined_data.csv", row.names = TRUE)

# ENVIRONMENTAL DATA SET IMPORT AND CONVERT TO DATA FRAME
Air_pollution_deaths_breakdown_by_age_IHME <- read_csv("Working_Data/environmental/Air pollution deaths breakdown by age - IHME.csv")
Air_pollution_deaths_breakdown_by_age_IHME_df <- data.frame(Air_pollution_deaths_breakdown_by_age_IHME)
Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_ <- read_csv("Working_Data/environmental/Deaths attributed to air pollution (Lelieveld et al. 2019).csv")
Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df <- data.frame(Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_)

Air_pollution_deaths_breakdown_by_age_IHME_df <- Air_pollution_deaths_breakdown_by_age_IHME_df %>% group_by(Entity) %>% slice_max(Year) 
Air_pollution_deaths_breakdown_by_age_IHME_df_NO_YEAR <- subset(Air_pollution_deaths_breakdown_by_age_IHME_df, select= -c(Year))

Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df <- Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df %>% group_by(Entity) %>% slice_max(Year)
Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df_NO_YEAR <- subset(Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df, select= -c(Year))

Environmental_data <- merge(x = Air_pollution_deaths_breakdown_by_age_IHME_df_NO_YEAR, 
                            y = Deaths_attributed_to_air_pollution_Lelieveld_et_al_2019_df_NO_YEAR , by = "Entity", all.x = TRUE)
View(Environmental_data)

# CREATE CSV FILE FOR DATA FRAME FOR ENVIRONMENT

write.csv(Environmental_data, "Environmental_data.csv", row.names = TRUE)


#owid_latest_df_Entity <- setNames(df1, replace(names(owid_latest_df), names(owid_latest_df) == 'location', 'Entity'))
#Environmental_data_and_covid_deaths_combined <- merge(x = owid_latest_df_Entity, 
#                                                       y = Environmental_data , by = "Entity", all.x = TRUE)
#View(Environmental_data_and_covid_deaths_combined)
# NEED TO TRY AND GET IT TO GROUP/MERGE BY BOTH ENTITY AND YEAR - CURRENTLY ONLY MERGING BY ENTITY



