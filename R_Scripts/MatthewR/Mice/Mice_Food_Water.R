# Trying MICE on just food and water data





food_water_df <- read_csv("Combined DataFrame Work/food_water_df.csv")
food_water_df <- subset(food_water_df, select = -1)
food_water_df <- clean_names(food_water_df)

food_water_imputation <- mice(data = food_water_df, m = 5, method = "cart", maxit = 50)

food_water_df1 <- complete(food_water_imputation, 1)
food_water_df2 <- complete(food_water_imputation, 2)
food_water_df3 <- complete(food_water_imputation, 3)
food_water_df4 <- complete(food_water_imputation, 4)
food_water_df5 <- complete(food_water_imputation, 5)

