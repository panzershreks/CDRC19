library(readr)
library(mice)
library(VIM)
library(janitor)
library(car)
library(MuMIn)
library(readr)
library(ggplot2)
library(naniar)
library(visdat)
library(Amelia)
library(ggplot2)
library(dplyr)
library(xtable)


healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")
healthcare_all_mis_with_entity <- subset(healthcare_all_mis, select = -1)

healthcare_all_mis <- subset(healthcare_all_mis, select = -c(1, 2))

healthcare_all_imputed <- read_csv("R_Scripts/EmmaR/healthcare_all_imputed.csv")
healthcare_all_imputed <- subset(healthcare_all_imputed, select = -1)

# Missing data plot

vis_miss(healthcare_all_mis) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Healthcare Variables Missingness Plot")

# Missing data table by variables 

missing_table <- miss_var_summary(healthcare_all_mis, sort_miss = TRUE)
missing_table <- data.frame(missing_table)
View(missing_table)

# Missing data table by entities 

miss_table_entity <- healthcare_all_mis_with_entity %>%
  group_by(entity) %>%
  miss_case_summary(order = TRUE) %>%
  order(miss_table_entity$pct_miss, decreasing = TRUE)

miss_table_entity <- miss_table_entity[order(miss_table_entity$pct_miss, decreasing = FALSE),]

View(miss_table_entity)

write.csv(miss_table_entity, file = "miss_table_healthcare_entity.csv")




# Compute correlation plot

corr <-healthcare_all_imputed

healthcare_corr <- vis_cor(corr) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Healthcare Variables Correlation Plot")

healthcare_corr



