library('xlsx')

library('missForest')
read.xlsx('number check')

# clean_demographic data
demographic_data <- read.csv(file = 'clean_demographic.csv')

# A function find the missing ratio
pMiss <- function(x) {sum(is.na(x))/length(x)*100}

# Colomn missing ratio
apply(demographic_data,2,pMiss)

# Row missing ratio
apply(demographic_data,1,pMiss)


# Check the data
summary(demographic_data)


# Data we need 
A <- subset(demographic_data, select = c(4:23))

A <- clean_names(A)

which(A == 'invesetment_vaccines', arr.ind=TRUE)

# Missdata using random forest
dat2 <- missForest(A,ntree = 100)

# The data we recover
B = dat2$ximp

# Colomn missing ratio
apply(B,2,pMiss)

# Row missing ratio
apply(B,1,pMiss)


x
data$stringency_index

# normalized root mean squared error computed (NRMSE)
dat2$OOBerror

# Model selection




# Linear regression
fit1 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
     B$Religious.compatibility..Larson.et.al..2016.. + 
     B$Child.mortality.estimates..Gapminder..2015.. + 
     B$Child.mortality.1950.2017..IHME..2018..x + 
     B$Child.mortality.1950.2017..IHME..2018..y + 
     B$Inequality.in.life.expectancy..2015.2020. + 
     B$Inequality.in.education..2017. + 
     B$Infant.mortality.rate + 
     B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
     B$Life.expectancy + 
     B$Population.x + 
     B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
     B$Mean.BMI..male. + 
     B$Mean.BMI..female. + 
     B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
       B$Number.of.child.deaths.1950.2017..IHME..2017. + 
       B$Number.of.infant.deaths..IHME...2017. + 
       B$Number.of.neonatal.deaths..IHME...2017. + 
       B$Total.population..Gapminder..HYDE...UN. + 
       B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN. + 
       B$Share.of.world.population)
library(olsrr)

ols_vif_tol(fit1)
tstep <- step(fit1)

# Use Vif to get rid of data

# New model
fit2 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Child.mortality.1950.2017..IHME..2018..x + 
             B$Child.mortality.1950.2017..IHME..2018..y + 
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.child.deaths.1950.2017..IHME..2017. + 
             B$Number.of.infant.deaths..IHME...2017. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN. + 
             B$Share.of.world.population)
ols_vif_tol(fit2)
tstep <- step(fit1)

#New model

fit3 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Child.mortality.1950.2017..IHME..2018..x +
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.child.deaths.1950.2017..IHME..2017. + 
             B$Number.of.infant.deaths..IHME...2017. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN. + 
             B$Share.of.world.population)

ols_vif_tol(fit3)
tstep <- step(fit1)

# New model 


fit4 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Child.mortality.1950.2017..IHME..2018..x +
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.child.deaths.1950.2017..IHME..2017. + 
             B$Number.of.infant.deaths..IHME...2017. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit4)
tstep <- step(fit1)


# New model

fit5 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Child.mortality.1950.2017..IHME..2018..x +
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.child.deaths.1950.2017..IHME..2017. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit5)
tstep <- step(fit1)

# New model

fit6 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Child.mortality.1950.2017..IHME..2018..x +
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit6)
tstep <- step(fit1)

# New model

fit7 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit7)
tstep <- step(fit1)


# New model


fit8 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit8)
tstep <- step(fit1)


# New model


fit9 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit9)
tstep <- step(fit1)


# New model


fit10 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Population.x + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Number.of.neonatal.deaths..IHME...2017. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit10)
tstep <- step(fit1)

# New model


fit11 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
              B$Religious.compatibility..Larson.et.al..2016.. + 
              B$Inequality.in.education..2017. + 
              B$Infant.mortality.rate + 
              B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
              B$Life.expectancy + 
              B$Population.x + 
              B$Mean.BMI..male. + 
              B$Mean.BMI..female. + 
              B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
              B$Number.of.neonatal.deaths..IHME...2017.)

ols_vif_tol(fit11)
tstep <- step(fit11, direction="backward")


tstep1 <- step(fit11, direction="both")


tstep2 <- step(fit11, direction="forward")
#


fit12 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~
              B$Life.expectancy + 
              B$Mean.BMI..male.)
summary(fit12)

# New model
fit3 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
             B$Religious.compatibility..Larson.et.al..2016.. + 
             B$Child.mortality.estimates..Gapminder..2015.. + 
             B$Inequality.in.life.expectancy..2015.2020. + 
             B$Inequality.in.education..2017. + 
             B$Infant.mortality.rate + 
             B$International.Historical.Statistics..Deaths.per.1.000...Brian.Mitchell..2013.. + 
             B$Life.expectancy + 
             B$Life.Expectancy..1950.2015...UN.Population.Division..2015.. + 
             B$Mean.BMI..male. + 
             B$Mean.BMI..female. + 
             B$Neonatal.Mortality.Rate..via.Childmortality.org..2015.. + 
             B$Population.by.country.and.region..historic.and.projections..Gapminder..HYDE...UN.)

ols_vif_tol(fit3)
tstep <- step(fit1)

# Summary the lm


# Data we need 
food_water_df <- read.csv(file = "food_water_df.csv")
food_water_df <- subset(food_water_df, select = -1)
food_water_df <- clean_names(food_water_df)

# Missdata using random forest
dat3 <- missForest(food_water_df,ntree = 100)

# The data we recover
C = dat3$ximp

# normalized root mean squared error computed (NRMSE)
dat3$OOBerror

# Linear regression
lm(C$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ C$stringency_index)

# Summary the lm
summary(lm(C$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ C$stringency_index))
