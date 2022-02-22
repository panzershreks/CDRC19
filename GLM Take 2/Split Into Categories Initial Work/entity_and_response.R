
# Get entity and response variables and output to CSV for future retrieval

econ_data_full <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_economic.csv")
econ_data_full <- clean_names(econ_data_full)
clean_econ <- econ_data_full %>% drop_na(total_confirmed_deaths_due_to_covid_19_per_million_people)
entity_and_response_col <- subset(clean_econ, select = c(entity, total_confirmed_deaths_due_to_covid_19_per_million_people))

write.csv(entity_and_response_col, file="GLM Take 2/Split Into Categories Initial Work/entity_and_response.csv", row.names=FALSE)