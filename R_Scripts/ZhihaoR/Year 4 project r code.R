library('xlsx')
library(olsrr)
library('missForest')
library(car)
library(janitor)

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
A <- subset(demographic_data, select = c(3:23))

A <- clean_names(A)

which(A == 'invesetment_vaccines', arr.ind=TRUE)

# Missdata using random forest
dat2 <- missForest(A,ntree = 200,maxiter = 15)

# The data we recover
B = dat2$ximp

C = c(sample(1:193,133,replace = FALSE))
A = c(1:193)


# 30 percent test 70 percent train

Train_set = B[C,]

Test_set = B[-C,]

# Colomn missing ratio
apply(B,2,pMiss)

# Row missing ratio
apply(B,1,pMiss)

# normalized root mean squared error computed (NRMSE)
dat2$OOBerror

#' Iteratively drop variables based on GVIF
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @param vif_max num for max VIF allowed (not GVIF)
#' @return list of str of explanatory variables after dropping
gvif_drop <- function(resp_var, expl_var, data, vif_max=5) {
  gvif_max <- vif_max ^ 0.5
  lm_formula <- lm_formula_paster(resp_var, expl_var)
  model <- lm(lm_formula, data)
  vif_mod <- vif(model)
  try(gvif <- vif_mod, silent = TRUE)
  try(gvif <- vif_mod[,3], silent = TRUE)
  if (is.null(dim(vif_mod))) {
    gvif_max <- vif_max
  }
  while (max(gvif) > gvif_max) {
    expl_var <- expl_var[-(which.max(gvif))]
    lm_formula <- lm_formula_paster(resp_var, expl_var)
    model <- lm(lm_formula, data)
    vif_mod <- vif(model)
    try(gvif <- vif_mod, silent = TRUE)
    try(gvif <- vif_mod[,3], silent = TRUE)
    if (is.null(dim(vif_mod))) {
      gvif_max <- vif_max
    }
  }
  return (expl_var)
}



#' create a lm formula from list of variables
#' helper function for gvif_drop
#' @param resp_var str of response variable
#' @param expl_var list of str of explanatory variables
#' @return str of formula using the variables provided
lm_formula_paster <- function(resp_var, expl_var) {
  form <- paste0(resp_var, "~")
  for (var in head(expl_var, -1)) {
    form <- paste0(form, var, "+")
  }
  form <- paste0(form, tail(expl, 1))
  return (form)
}

# Using Random forest built-in function to figure out missing values
# Model selection
resp <- "total_confirmed_deaths_due_to_covid_19_per_million_people"

expl <- c("religious_compatibility_larson_et_al_2016",
          "child_mortality_estimates_gapminder_2015",
          "child_mortality_1950_2017_ihme_2018_x",
          "child_mortality_1950_2017_ihme_2018_y",
          "inequality_in_life_expectancy_2015_2020",
          "inequality_in_education_2017",
          "infant_mortality_rate",
          "international_historical_statistics_deaths_per_1_000_brian_mitchell_2013",
          "life_expectancy",
          "population_x",
          "life_expectancy_1950_2015_un_population_division_2015",
          "mean_bmi_male",
          "mean_bmi_female",
          "neonatal_mortality_rate_via_childmortality_org_2015",
          "number_of_child_deaths_1950_2017_ihme_2017",
          "number_of_infant_deaths_ihme_2017",
          "number_of_neonatal_deaths_ihme_2017",
          "total_population_gapminder_hyde_un",
          "population_by_country_and_region_historic_and_projections_gapminder_hyde_un",
          "share_of_world_population")

after_drop <- gvif_drop(resp, expl, B)
final_formula <- lm_formula_paster(resp, after_drop)
final_model <- lm(final_formula,B)
vif(final_model)

# backward selection
tstep1 <- step(final_model, direction="backward")

# Assessment
summary(tstep1)


fit12 <- lm(demographic_data$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~
              B$Life.expectancy + 
              B$Mean.BMI..male.)
summary(fit12)


# Ridge regression
library(MASS)
library(glmnet)

y <- Train_set$total_confirmed_deaths_due_to_covid_19_per_million_people
x <- as.matrix(Train_set[,2:21])

ridge<- glmnet(x,y,family = "gaussian",alpha = 0)
print(ridge)

plot(ridge)

plot(ridge,xvar = "lambda",label = TRUE)


newx <- as.matrix(Test_set[,2:21])

# Analysis
ridge.y <- predict(ridge,newx = newx,type = "response",s = 8)
library(InformationValue)
 plot(ridge.y,Test_set$total_confirmed_deaths_due_to_covid_19_per_million_people)

index = Test_set$total_confirmed_deaths_due_to_covid_19_per_million_people <300

H = Test_set$total_confirmed_deaths_due_to_covid_19_per_million_people[index]

ridge.y <- ridge.y[index]

plot(ridge.y,H)

ridge.sol<-lm.ridge(B$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
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
                      B$Share.of.world.population,lambda = seq(0,10,length = 101))

names(ridge.sol)

ridge.sol$lambda[which.min(ridge.sol$GCV)]

ridge.sol$coef[which.min(ridge.sol$GCV)]

par(mfrow = c(1, 1))


plot(ridge.sol,xvar = "lambda",label = TRUE)

matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients", 
        type = "l", lty = 1:20)

abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(beta))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])

library(ridge)

mod <- linearRidge(B$Total.confirmed.deaths.due.to.COVID.19.per.million.people ~ 
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

summary(mod)




word_data <- read.csv(file = 'clean_world_stats.csv')

# A function find the missing ratio
pMiss <- function(x) {sum(is.na(x))/length(x)*100}

# Colomn missing ratio
apply(word_data,2,pMiss)

# Row missing ratio
apply(word_data,1,pMiss)


# Check the data
summary(word_data)


# Data we need 
D <- subset(word_data, select = c(3:4))

library(janitor)
A <- clean_names(A)

which(A == 'invesetment_vaccines', arr.ind=TRUE)

# Missdata using random forest
dat3 <- missForest(D,ntree = 100,maxiter = 15)


B

# Final data frame

Final_data <- data.frame(B$life_expectancy,B$mean_bmi_female)

Mean_finaldata <- colMeans(Final_data)
Cov_finaldata <- cov(Final_data)

## MCMC inference

# MH algorithm

# Symmetry proposal

n.rep <- 10000; n.accept <- 0
theta <- matrix(0,2,n.rep) ## storage for sim. values
ll0 <- sum(dnorm(x,mean=theta[1,1],
                 sd=exp(theta[2,1]),log=TRUE))
for (i in 2:n.rep) { ## The MH loop
  theta[,i] <- theta[,i-1] + rt(2,df=3)*.5 ## proposal
  ll1 <- sum(dnorm(x,mean=theta[1,i],
                   sd=exp(theta[2,i]),log=TRUE))
  if (exp(ll1-ll0)>runif(1)) { ## MH accept/reject
    ll0 <- ll1; n.accept <- n.accept + 1 ## accept
  } else theta[,i] <- theta[,i-1] ## reject
}
n.accept/n.rep ## proportion of proposals accepted
