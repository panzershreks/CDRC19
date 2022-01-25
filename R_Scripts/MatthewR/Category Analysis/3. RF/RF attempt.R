# Trying random forests

library('missForest')

Name <- c("A", "B", "C", "D", "E", "F")
Age <- c(23, 41, 32, 58, 26, 27)
Height <- c(150,120,140, NA,150, 100)

df <- data.frame(Name, Age, Height)

missForest(df,ntree = 100)
