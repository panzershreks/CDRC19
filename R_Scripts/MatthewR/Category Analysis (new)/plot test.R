
total <- rf_covid_imputed_data_variables$total_confirmed_deaths_due_to_covid_19

per_person <- matthew_sig_var_complete$total_confirmed_deaths_due_to_covid_19_per_million_people

plot(total, per_person)


cor(total, per_person)
