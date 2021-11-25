# Here we define the factor variables.

fully_merged_df
fully_merged_df <- subset(fully_merged_df, select = -1)
fully_merged_df <- clean_names(fully_merged_df)



# Here, we code in the factor variables.

fully_merged_df$school_closures <- as.factor(fully_merged_df$school_closures)

fully_merged_df$workplace_closures <- as.factor(fully_merged_df$workplace_closures)

fully_merged_df$cancel_public_events <- as.factor(fully_merged_df$cancel_public_events)

fully_merged_df$close_public_transport <- as.factor(fully_merged_df$close_public_transport)

fully_merged_df$public_information_campaigns <- as.factor(fully_merged_df$public_information_campaigns)

fully_merged_df$restrictions_internal_movements <- as.factor(fully_merged_df$restrictions_internal_movements)

fully_merged_df$international_travel_controls <- as.factor(fully_merged_df$international_travel_controls)

fully_merged_df$fiscal_measures <- as.factor(fully_merged_df$fiscal_measures)

fully_merged_df$emergency_investment_healthcare <- as.factor(fully_merged_df$emergency_investment_healthcare)

fully_merged_df$investment_vaccines <- as.factor(fully_merged_df$investment_vaccines)

fully_merged_df$contact_tracing <- as.factor(fully_merged_df$contact_tracing)

fully_merged_df$restriction_gatherings <- as.factor(fully_merged_df$restriction_gatherings)

fully_merged_df$stay_home_requirements <- as.factor(fully_merged_df$stay_home_requirements)

fully_merged_df$income_support <- as.factor(fully_merged_df$income_support)

fully_merged_df$debt_relief <- as.factor(fully_merged_df$debt_relief)

fully_merged_df$international_support <- as.factor(fully_merged_df$international_support)

fully_merged_df$testing_policy <- as.factor(fully_merged_df$testing_policy)

fully_merged_df$facial_coverings <- as.factor(fully_merged_df$facial_coverings)

fully_merged_df$vaccination_policy <- as.factor(fully_merged_df$vaccination_policy)

fully_merged_df$vaccine_eligibility <- as.factor(fully_merged_df$vaccine_eligibility )


