#### List of Countries
library(janitor)

source("LM Data and Analysis/list_of_new_countries.R")

clean_fully_merged <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_fully_merged.csv")
clean_fully_merged <- clean_names(clean_fully_merged)
clean_fully_merged <- clean_fully_merged[-c(20,29,48,54,56,67,88,91,106,112,118,125,126,130,142,143,144,
                                            145, 151, 156, 171,173,177,178, 186,193),]
clean_fully_merged <- subset(clean_fully_merged, select = -1)

# Access Response Variable

response_variable <- clean_fully_merged[,57]

report_list <- cbind(1:167, list_of_countries, response_variable)
colnames(report_list) <- c("Number", "Entity","Total Confirmed Deaths due to Covid-19 per Million")
write.csv(report_list,"report_list_of_countries.csv", row.names = TRUE)

