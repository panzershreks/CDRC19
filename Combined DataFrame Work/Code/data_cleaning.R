library("ggplot2")
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)

fully_merged_df <- read_csv("Combined DataFrame Work/CSV Files/fully_merged_df.csv")

fully_merged_df <- subset(fully_merged_df, select = -1)

# clean_fully_merged_df <- fully_merged_df[-c('Americas'),]

clean_fully_merged_df <- filter(fully_merged_df, !(Entity %in% c("American Samoa", "Americas", "Anguilla", "Aruba", "Bonaire", "Bonaire Sint Eustatius and Saba","British Virgin Islands","Caribbean", "Cayman Islands","Central America", 
                                                                 "Central America and Mexico", "Channel Islands", "Cook Islands", "Curacao","Cura�ao", "Czechoslovakia", "England", "Eswatini",
                                                                 "Faeroe Islands", "Falkland Islands", "Faroe Islands","Former USSR", "French Guiana", "French Polynesia", "Gibraltar","Great Britain", "Greenland", "Guadeloupe", 
                                                                 "Guam", "Guernsey", "Hawaii", "Hong Kong", "India (Urban)", "Isle of Man", 
                                                                 "Jersey", "Kosovo", "Macao", "Macau", "Macedonia", "Malaya", "Martinique", "Mayotte", "Mayotte", "Montserrat", "NAFTA", "Netherlands Antilles", "New Caledonia", 
                                                                 "Niue", "Korea", "Northern Ireland", "Northern Mariana Islands", "Palestine", "Polynesia", "Puerto Rico", 
                                                                 "Saint Barthelemy", "Saint Barthlemy", "Saint Helena", "Saint Martin", "Saint Martin (French part)", "Saint Pierre and Miquelon", "Scotland", "Serbia and Montenegro", "Sint Maarten", "Sint Maarten (Dutch part)", 
                                                                 "Slovak Republic", "Somaliland region", "Svalbard and Jan Mayen", "Taiwan", "The Bahamas", "Timor-Leste", "Tokelau", "Turks and Caicos Islands", "United States Virgin Islands", "Vatican", "Vatican City", 
                                                                 "Virgin Islands", "Virgin Islands, U.S.", "Wales", "Wallis and Futuna", "Western Asia", "World Bank Upper Middle Income", "Cabo Verde", "Ivory Coast", "DR Congo", "Republic of the Congo", 
                                                                 "Federated States of Micronesia", "Micronesia", "Bermuda", "Melanesia", )))

# Need to combine czechia and czech republic and add luxembourg 
clean_fully_merged_df