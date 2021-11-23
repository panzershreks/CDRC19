# Taking a look at the MCIE package
library('mice')

a <- c(2,3,1)
b <- c(10, 15, 12)
c <- c(NA,13,18)

test_data_frame <- data.frame(a,b,c)

mice(test_data_frame)
