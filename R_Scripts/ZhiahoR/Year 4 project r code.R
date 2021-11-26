library('xlsx')
read.xlsx('number check')
data <- read.csv(file = 'fully_merged_df.csv')

# A function find the missing ratio
pMiss <- function(x) {sum(is.na(x))/length(x)*100}

# Colomn missing ratio
apply(data,2,pMiss)

# Row missing ratio
apply(data,1,pMiss)


# Check the data
summary(data)

# Data we need 
A <- subset(data,select = c(14,55,59))
xxx
# Missdata using random forest
dat2 <- missForest(A,ntree = 100)

# The data we recover
C = dat2$ximp

# normalized root mean squared error computed (NRMSE)
dat2$OOBerror

# Linear regression
lm(C$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ C$stringency_index)

# Summary the lm
summary(lm(C$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ C$stringency_index))