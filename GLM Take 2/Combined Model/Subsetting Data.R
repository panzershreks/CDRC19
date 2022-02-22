library(readr)
combined_all_missing <- read_csv("GLM Take 2/Combined Model/combined_all_missing.csv")
combined_all_missing <- subset(combined_all_missing, select = -1)


# List to remove from the dataframes

combined_all_missing <- subset(combined_all_missing, select = -c(2:12, 14:16, 18, 21:43, 45, 46, 52, 54, 56, 57, 62, 
                                                                 67:73, 75:77, 80:83, 85, 88, 89, 91:94, 96:102, 104:108, 111:114))


write.csv(combined_all_missing,"GLM Take 2/Combined Model/subset_of_total.csv", row.names = TRUE)






