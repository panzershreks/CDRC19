library("ggplot2")
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library(plyr)

fully_merged_df <- read_csv("Combined DataFrame Work/CSV Files/fully_merged_df.csv")

fully_merged_df <- subset(fully_merged_df, select = -1)


clean_fully_merged_df <- filter(fully_merged_df, !(Entity %in% c("American Samoa", "Americas", "Anguilla", "Aruba", "Bonaire", "Bonaire Sint Eustatius and Saba","British Virgin Islands","Caribbean", "Cayman Islands","Central America", 
                                                                 "Central America and Mexico", "Channel Islands", "Cook Islands", "Curacao","Cura�ao", "Czechoslovakia", "England",
                                                                 "Faeroe Islands", "Falkland Islands", "Faroe Islands","Former USSR", "French Guiana", "French Polynesia", "Gibraltar","Great Britain", "Greenland", "Guadeloupe", 
                                                                 "Guam", "Guernsey", "Hawaii", "Hong Kong", "India (Urban)", "Isle of Man", 
                                                                 "Jersey", "Kosovo", "Macao", "Macau", "Macedonia", "Malaya", "Martinique", "Mayotte", "Mayotte", "Montserrat", "NAFTA", "Netherlands Antilles", "New Caledonia", 
                                                                 "Niue", "Korea", "Northern Ireland", "Northern Mariana Islands", "Palestine", "Polynesia", "Puerto Rico", 
                                                                 "Saint Barthelemy", "Saint Barthlemy", "Saint Helena", "Saint Martin", "Saint Martin (French part)", "Saint Pierre and Miquelon", "Scotland", "Serbia and Montenegro", "Sint Maarten", "Sint Maarten (Dutch part)", 
                                                                 "Slovak Republic", "Somaliland region", "Svalbard and Jan Mayen", "Taiwan", "The Bahamas", "Timor-Leste", "Tokelau", "Turks and Caicos Islands", "United States Virgin Islands", "Vatican", "Vatican City", 
                                                                 "Virgin Islands", "Virgin Islands, U.S.", "Wales", "Wallis and Futuna", "Western Asia", "World Bank Upper Middle Income", "Cabo Verde", "Ivory Coast", "DR Congo", "Republic of the Congo", 
                                                                 "Bermuda", "Melanesia","Africa","Andean Latin America","Angola (Urban)", "Arab States", "Arab World","Argentina (Urban)", "Asia", "Asia excl. China", "Australasia","Australia - Australian Capital Territory","Australia - New South Wales",
                                                                 "Australia/New Zealand","Bolivia (Urban)", "Canada - Alberta", "Canada - British Columbia", "Canada - Ontario", "Caribbean small states",
                                                                 "Caribbean small states","Central Africa","Central Asia","Central Asia and North Africa-Middle East","Central Asia, Middle East and North Africa","Central Europe","Central Europe and the Baltics","Central Europe, Eastern Europe, and Central Asia",
                                                                 "Central Latin America","Central sub-Saharan Africa","Central Sub-Saharan Africa","China - Guangdong","China (Rural)","China (Urban)","Colombia (Urban)","Developing countries","Early-demographic dividend","East and South East Asia",
                                                                 "East Asia","East Asia & Pacific","East Asia & Pacific (all income levels)","East Asia & Pacific (developing only)","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries)", "East Asia & Pacific (IDA & IBRD)", "East Asia and the Pacific", "East Germany",
                                                                 "Eastern Africa","Eastern Asia","Eastern Europe","Eastern Mediterranean","Eastern sub-Saharan Africa","Eastern Sub-Saharan Africa","Ecuador (Urban)","England and Wales","Ethiopia (Rural)","EU","Euro Area","Europe","Europe & Central Asia",
                                                                 "Euro area","Europe & Central Asia (all income levels)","Europe & Central Asia (developing only)","Europe & Central Asia (excluding high income)",
                                                                 "Europe & Central Asia (IDA & IBRD countries)","Europe & Central Asia (IDA & IBRD)","Europe and Central Asia","European Union",
                                                                 "Former Yugoslavia","Fragile and conflict affected situations","G7","Heavily indebted poor countries (HIPC)","High human development",
                                                                 "High income","High income: nonOECD","High income: OECD","High SDI", "High-income","High-income Asia Pacific", "High-income Asia-Pacific",
                                                                 "High-income countries","High-income North America","High-income Western countries","High-middle SDI","Honduras (Urban)","IBRD only","IDA & IBRD total","IDA blend","IDA only",
                                                                 "IDA total","India (Rural)","Indonesia (Rural)","Indonesia (Urban)", "International","Landlocked developing countries","Late-demographic dividend",
                                                                 "Latin America", "Latin America & Caribbean","Latin America & Caribbean (all income levels)","Latin America & Caribbean (developing only)","Latin America & Caribbean (excluding high income)","Latin America & Caribbean (IDA & IBRD)","Latin America & the Caribbean (IDA & IBRD countries)","Latin America and Caribbean",
                                                                 "Latin America and the Caribbean","Least developed countries","Least developed countries: UN classification","Less developed regions","Less developed regions, excluding China","Less developed regions, excluding least developed countries","Low & middle income","Low human development",
                                                                 "Low income","Low income SDI","Low-income countries","Low-middle SDI","Lower middle income","Lower-middle-income countries","Medium human development","Micronesia (Urban)","Middle Africa","Middle East","Middle East & North Africa","Middle East & North Africa (all income levels)",
                                                                 "Middle East & North Africa (developing only)", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)","Middle East & North Africa (IDA & IBRD)", "Middle income", "Middle SDI",
                                                                 "Middle-income countries", "More developed regions", "Low SDI", "Newfoundland","North Africa","North Africa and Middle East","North America","Northern Africa","Northern America","Northern Cyprus", "Northern Europe",
                                                                 "Oceania","OECD - Total","OECD members", "Other small states", "Pacific island small states","Organization for Economic Co-operation and Development",
                                                                 "Post-demographic dividend","Pre-demographic dividend","Reunion","Small island developing states","Small island developing States", "Small states",
                                                                 "South Africa (non-whites)","South Africa (whites)","South America","South and South-East Asia","South and Southeast Asia","South Asia","South Asia (IDA & IBRD)",
                                                                 "South-Central Asia","South-East Asia","South-Eastern Asia","Southeast Asia","Southeast Asia, East Asia, and Oceania","Southern Africa", "Southern Asia",
                                                                 "Southern Europe", "Southern Latin America", "Southern sub-Saharan Africa", "Southern Sub-Saharan Africa", "Sub-Sahara Africa","Sub-Saharan Africa",
                                                                 "Sub-Saharan Africa (all income levels)","Sub-Saharan Africa (developing only)", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)","Sub-Saharan Africa (IDA & IBRD)",
                                                                 "Sudan (former)","Suriname (Urban)","Tropical Latin America","United States - CDC samples tested","United States - COVID-Tracking project","Upper middle income","Upper-middle-income countries",
                                                                 "Uruguay (Urban)", "USA (blacks)","USA (whites)","Very high human development", "West Asia", "West Germany", "Western Africa", "Western Europe","Western Offshoots", "Western Pacific", "Western Sahara", "Western sub-Saharan Africa", "Western Sub-Saharan Africa",
                                                                 "World", "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income", "World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore", "World no China", "Zimbabwe (whites only)", "Central and Eastern Europe",
                                                                 "Yugoslawia","Sudan and South Sudan","Swaziland")))


# We now combine the Czechia and Czech Rep. data and Fed. States of Micronesia and Micronesia


cr_hold <- clean_fully_merged_df %>% filter(Entity == "Czech Republic")
c_hold <- clean_fully_merged_df %>% filter(Entity == "Czechia")
c_row_df <- rbind(cr_hold, c_hold)
c_combined <- c_row_df %>% fill(everything(), .direction = "updown") %>% slice_head()

fm_hold <- clean_fully_merged_df %>% filter(Entity == "Federated States of Micronesia")
m_hold <- clean_fully_merged_df %>% filter(Entity == "Micronesia")
mc_hold <- clean_fully_merged_df %>% filter(Entity == "Micronesia (country)")
m_row_df <- rbind(fm_hold, m_hold,mc_hold)
m_combined <- m_row_df %>% fill(everything(), .direction = "updown") %>% slice_tail()



clean_fully_merged_df <- subset(clean_fully_merged_df, Entity != "Czech Republic" & Entity != "Czechia" & Entity != "Federated States of Micronesia" & Entity != "Micronesia" & Entity != "Micronesia (country)")
clean_fully_merged_df <- rbind(clean_fully_merged_df, c_combined)
clean_fully_merged_df <- rbind(clean_fully_merged_df, m_combined)

# write.csv(clean_fully_merged_df,"clean_fully_merged.csv", row.names = TRUE)

# Now we will do the same for each category:

# Covid DF

covid_df <- read_csv("Combined DataFrame Work/CSV Files/Not Clean By Category CSV Files/covid_df.csv")

fully_merged_df <- subset(fully_merged_df, select = -1)


clean_fully_merged_df <- filter(fully_merged_df, !(Entity %in% c("American Samoa", "Americas", "Anguilla", "Aruba", "Bonaire", "Bonaire Sint Eustatius and Saba","British Virgin Islands","Caribbean", "Cayman Islands","Central America", 
                                                                 "Central America and Mexico", "Channel Islands", "Cook Islands", "Curacao","Cura�ao", "Czechoslovakia", "England",
                                                                 "Faeroe Islands", "Falkland Islands", "Faroe Islands","Former USSR", "French Guiana", "French Polynesia", "Gibraltar","Great Britain", "Greenland", "Guadeloupe", 
                                                                 "Guam", "Guernsey", "Hawaii", "Hong Kong", "India (Urban)", "Isle of Man", 
                                                                 "Jersey", "Kosovo", "Macao", "Macau", "Macedonia", "Malaya", "Martinique", "Mayotte", "Mayotte", "Montserrat", "NAFTA", "Netherlands Antilles", "New Caledonia", 
                                                                 "Niue", "Korea", "Northern Ireland", "Northern Mariana Islands", "Palestine", "Polynesia", "Puerto Rico", 
                                                                 "Saint Barthelemy", "Saint Barthlemy", "Saint Helena", "Saint Martin", "Saint Martin (French part)", "Saint Pierre and Miquelon", "Scotland", "Serbia and Montenegro", "Sint Maarten", "Sint Maarten (Dutch part)", 
                                                                 "Slovak Republic", "Somaliland region", "Svalbard and Jan Mayen", "Taiwan", "The Bahamas", "Timor-Leste", "Tokelau", "Turks and Caicos Islands", "United States Virgin Islands", "Vatican", "Vatican City", 
                                                                 "Virgin Islands", "Virgin Islands, U.S.", "Wales", "Wallis and Futuna", "Western Asia", "World Bank Upper Middle Income", "Cabo Verde", "Ivory Coast", "DR Congo", "Republic of the Congo", 
                                                                 "Bermuda", "Melanesia","Africa","Andean Latin America","Angola (Urban)", "Arab States", "Arab World","Argentina (Urban)", "Asia", "Asia excl. China", "Australasia","Australia - Australian Capital Territory","Australia - New South Wales",
                                                                 "Australia/New Zealand","Bolivia (Urban)", "Canada - Alberta", "Canada - British Columbia", "Canada - Ontario", "Caribbean small states",
                                                                 "Caribbean small states","Central Africa","Central Asia","Central Asia and North Africa-Middle East","Central Asia, Middle East and North Africa","Central Europe","Central Europe and the Baltics","Central Europe, Eastern Europe, and Central Asia",
                                                                 "Central Latin America","Central sub-Saharan Africa","Central Sub-Saharan Africa","China - Guangdong","China (Rural)","China (Urban)","Colombia (Urban)","Developing countries","Early-demographic dividend","East and South East Asia",
                                                                 "East Asia","East Asia & Pacific","East Asia & Pacific (all income levels)","East Asia & Pacific (developing only)","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries)", "East Asia & Pacific (IDA & IBRD)", "East Asia and the Pacific", "East Germany",
                                                                 "Eastern Africa","Eastern Asia","Eastern Europe","Eastern Mediterranean","Eastern sub-Saharan Africa","Eastern Sub-Saharan Africa","Ecuador (Urban)","England and Wales","Ethiopia (Rural)","EU","Euro Area","Europe","Europe & Central Asia",
                                                                 "Euro area","Europe & Central Asia (all income levels)","Europe & Central Asia (developing only)","Europe & Central Asia (excluding high income)",
                                                                 "Europe & Central Asia (IDA & IBRD countries)","Europe & Central Asia (IDA & IBRD)","Europe and Central Asia","European Union",
                                                                 "Former Yugoslavia","Fragile and conflict affected situations","G7","Heavily indebted poor countries (HIPC)","High human development",
                                                                 "High income","High income: nonOECD","High income: OECD","High SDI", "High-income","High-income Asia Pacific", "High-income Asia-Pacific",
                                                                 "High-income countries","High-income North America","High-income Western countries","High-middle SDI","Honduras (Urban)","IBRD only","IDA & IBRD total","IDA blend","IDA only",
                                                                 "IDA total","India (Rural)","Indonesia (Rural)","Indonesia (Urban)", "International","Landlocked developing countries","Late-demographic dividend",
                                                                 "Latin America", "Latin America & Caribbean","Latin America & Caribbean (all income levels)","Latin America & Caribbean (developing only)","Latin America & Caribbean (excluding high income)","Latin America & Caribbean (IDA & IBRD)","Latin America & the Caribbean (IDA & IBRD countries)","Latin America and Caribbean",
                                                                 "Latin America and the Caribbean","Least developed countries","Least developed countries: UN classification","Less developed regions","Less developed regions, excluding China","Less developed regions, excluding least developed countries","Low & middle income","Low human development",
                                                                 "Low income","Low income SDI","Low-income countries","Low-middle SDI","Lower middle income","Lower-middle-income countries","Medium human development","Micronesia (Urban)","Middle Africa","Middle East","Middle East & North Africa","Middle East & North Africa (all income levels)",
                                                                 "Middle East & North Africa (developing only)", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)","Middle East & North Africa (IDA & IBRD)", "Middle income", "Middle SDI",
                                                                 "Middle-income countries", "More developed regions", "Low SDI", "Newfoundland","North Africa","North Africa and Middle East","North America","Northern Africa","Northern America","Northern Cyprus", "Northern Europe",
                                                                 "Oceania","OECD - Total","OECD members", "Other small states", "Pacific island small states","Organization for Economic Co-operation and Development",
                                                                 "Post-demographic dividend","Pre-demographic dividend","Reunion","Small island developing states","Small island developing States", "Small states",
                                                                 "South Africa (non-whites)","South Africa (whites)","South America","South and South-East Asia","South and Southeast Asia","South Asia","South Asia (IDA & IBRD)",
                                                                 "South-Central Asia","South-East Asia","South-Eastern Asia","Southeast Asia","Southeast Asia, East Asia, and Oceania","Southern Africa", "Southern Asia",
                                                                 "Southern Europe", "Southern Latin America", "Southern sub-Saharan Africa", "Southern Sub-Saharan Africa", "Sub-Sahara Africa","Sub-Saharan Africa",
                                                                 "Sub-Saharan Africa (all income levels)","Sub-Saharan Africa (developing only)", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)","Sub-Saharan Africa (IDA & IBRD)",
                                                                 "Sudan (former)","Suriname (Urban)","Tropical Latin America","United States - CDC samples tested","United States - COVID-Tracking project","Upper middle income","Upper-middle-income countries",
                                                                 "Uruguay (Urban)", "USA (blacks)","USA (whites)","Very high human development", "West Asia", "West Germany", "Western Africa", "Western Europe","Western Offshoots", "Western Pacific", "Western Sahara", "Western sub-Saharan Africa", "Western Sub-Saharan Africa",
                                                                 "World", "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income", "World excl. China","World excl. China and South Korea","World excl. China, South Korea, Japan and Singapore", "World no China", "Zimbabwe (whites only)", "Central and Eastern Europe",
                                                                 "Yugoslawia","Sudan and South Sudan","Swaziland")))


# We now combine the Czechia and Czech Rep. data and Fed. States of Micronesia and Micronesia


cr_hold <- clean_fully_merged_df %>% filter(Entity == "Czech Republic")
c_hold <- clean_fully_merged_df %>% filter(Entity == "Czechia")
c_row_df <- rbind(cr_hold, c_hold)
c_combined <- c_row_df %>% fill(everything(), .direction = "updown") %>% slice_head()

fm_hold <- clean_fully_merged_df %>% filter(Entity == "Federated States of Micronesia")
m_hold <- clean_fully_merged_df %>% filter(Entity == "Micronesia")
mc_hold <- clean_fully_merged_df %>% filter(Entity == "Micronesia (country)")
m_row_df <- rbind(fm_hold, m_hold,mc_hold)
m_combined <- m_row_df %>% fill(everything(), .direction = "updown") %>% slice_tail()



clean_fully_merged_df <- subset(clean_fully_merged_df, Entity != "Czech Republic" & Entity != "Czechia" & Entity != "Federated States of Micronesia" & Entity != "Micronesia" & Entity != "Micronesia (country)")
clean_fully_merged_df <- rbind(clean_fully_merged_df, c_combined)
clean_fully_merged_df <- rbind(clean_fully_merged_df, m_combined)


