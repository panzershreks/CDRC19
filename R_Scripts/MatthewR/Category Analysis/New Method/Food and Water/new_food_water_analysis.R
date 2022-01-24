# We load the packages we required

library("ggplot2")
library(grid)
library(gridExtra)
library(lattice)
library("tidyverse")
library("readr")
library(dplyr)
library(naniar)
library(car)
library(visdat)
library("mice")
library(UpSetR)
library("janitor")
library(corrplot)
library(gt)

# We read in and tidy the data we require.

clean_food_water <- read_csv("Combined DataFrame Work/CSV Files/Clean/clean_food_water.csv")
clean_food_water <- clean_names(clean_food_water)
clean_food_water <- subset(clean_food_water, select = -1)

# We look at the missing data and visualise it.

missing_table <- miss_var_summary(clean_food_water, sort_miss = TRUE)
food_missing_vis <- vis_miss(clean_food_water, sort_miss = TRUE) + theme(axis.text.x = element_text(angle = 90))
ggsave(food_missing_vis, file="food_missing_vis.png", height = 8 )

# We now carry out MICE Imputation, and we use the PMM method.

# The PMM method does not work in this case, and a suggested online way to get around it is to use the CART method, 
# so we use that instead.

set.seed(100)
food_water_imputation <- mice(data = clean_food_water, m = 5, method = c("cart"), maxit = 100)

food_water_imputation$loggedEvents

# We have that there are a few constant/collinear columns, so we will now deal with them
# by removing them.

food_water_1 <- complete(food_water_imputation, 1)
food_water_2 <- complete(food_water_imputation, 2)
food_water_3 <- complete(food_water_imputation, 3)
food_water_4 <- complete(food_water_imputation, 4)
food_water_5 <- complete(food_water_imputation, 5)

# now to proceed we will remove columns but we will from now on work with only
# food_water_1 data - 6,7,8 are collinear columns.

working_food_1 <- food_water_1
working_food_1 <- subset(working_food_1, select = -c(6,7,8))

working_food_2 <- food_water_2
working_food_2 <- subset(working_food_2, select = -c(6,7,8))

working_food_3 <- food_water_3
working_food_3 <- subset(working_food_3, select = -c(6,7,8))

working_food_4 <- food_water_4
working_food_4 <- subset(working_food_4, select = -c(6,7,8))

working_food_5 <- food_water_5
working_food_5 <- subset(working_food_5, select = -c(6,7,8))



# We now take the first imputed dataset and observe the correlation bewteen the variables.

Mcor <- working_food_1[,2:16]
f_w_cor_matrix <- vis_cor(Mcor) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Correlation Matrix")

cor_data_frame <- round(cor(Mcor),2)
# write.csv(cor_data_frame,"food_wat_correlation.csv", row.names = TRUE)


#




