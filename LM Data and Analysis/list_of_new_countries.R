# countries list

library(readr)
clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- subset(clean_fully_merged, select = -1)

clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
    145, 151, 156, 171,173,177,178, 186,193),]

list_of_countries <- subset(clean_fully_merged, select = 1)




# Models... these are the outliers
# what variables are causing this to happen


list_of_countries[110,]
list_of_countries[149,]
list_of_countries[27,]
list_of_countries[163,]











