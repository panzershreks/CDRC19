library(readr)
demographic_data <- read.csv(file = 'clean_demographic.csv')

clean_fully_merged <- demographic_data %>% drop_na(Total.confirmed.deaths.due.to.COVID.19.per.million.people)

list_of_countries <- subset(clean_fully_merged, select = 2)
list_of_countries
