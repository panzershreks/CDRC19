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


healthcare_all_mis <- read_csv("GLM Data and Analysis/Category CSV/healthcare_all_mis.csv")
healthcare_all_mis <- subset(healthcare_all_mis, select = -c(1, 2))

healthcare_all_imputed <- read_csv("R_Scripts/EmmaR/healthcare_all_imputed.csv")
healthcare_all_imputed <- subset(healthcare_all_imputed, select = -1)

# Missing data plot

vis_miss(healthcare_all_mis) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Healthcare Variables Missingness Plot")

# Compute correlation plot

corr <-healthcare_all_imputed

healthcare_corr <- vis_cor(corr) + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Healthcare Variables Correlation Plot")

healthcare_corr



