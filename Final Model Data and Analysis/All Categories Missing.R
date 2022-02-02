# Combining the Significant Variables with Missing Data Together

library(readr)
library("janitor")


# Matthew's Data

matthew_sig_var_w_missing <- read_csv("Final Model Data and Analysis/Categories with Missing/matthew_sig_var_w_missing.csv")
matthew_sig_var_w_missing <- clean_names(matthew_sig_var_w_missing)
matthew_sig_var_w_missing <- subset(matthew_sig_var_w_missing, select = -1)

# Emma's Data

healthcare_sigvars_missing <- read_csv("Final Model Data and Analysis/Categories with Missing/healthcare_sigvars_missing.csv")
healthcare_sigvars_missing <- clean_names(healthcare_sigvars_missing)
healthcare_sigvars_missing <- subset(healthcare_sigvars_missing, select = -1)

# Andy's Demographic Data - update this with the new data...

With_Missing_data_demographic_ <- read_csv("Final Model Data and Analysis/Categories with Missing/With_Missing data(demographic).csv")
With_Missing_data_demographic_ <- clean_names(With_Missing_data_demographic_)
With_Missing_data_demographic_ <- subset(With_Missing_data_demographic_, select = -1)

# Eman's Economic Data

econ_significant_pre_random_forest <- read_csv("Final Model Data and Analysis/Categories with Missing/econ_significant_pre_random_forest.csv")
econ_significant_pre_random_forest <- clean_names(econ_significant_pre_random_forest)
econ_significant_pre_random_forest <- subset(econ_significant_pre_random_forest, select = -1)







