---
title: "Wine Quality"
author: "First Last"
date: "11/27/2020"
output: html_document
---

## Red Wine Quality
```{r red}
# install required packages
library(MASS)
library(car)
library(readr)
# import data for red wine
red <- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(red)
summary(red)

# using multiple linear regression to start
lm.fit=lm(quality~., data=red)
lm.fit
# look at all variables in lm.fit object
names(lm.fit)
# summarize

# TODO: GET OUT OLD NOTES AND EXAMINE THESE RESULTS

summary(lm.fit)
# note that small p-value indicates significance of multiple independent variables

# plot the model itself
plot(lm.fit)

# TODO: WHAT IS GOING ON WITH THESE BONKERS GRAPHS
# Note that the Residuals Vs. Leverage graph is......... interesting
quality_red = red$quality

# look at distribution of quality of red wine throughout dataset
summary(red$quality)

# It appears that the most significant variables are
# volatile acidity
# chlorides
# total sulfur dioxide
# sulphates
# alcohol

plot(quality_red, red$`volatile acidity`, col='darkred')
plot(quality_red, red$chlorides, col='darkred')
plot(quality_red, red$`total sulfur dioxide`, col='darkred')
plot(quality_red, red$sulphates, col='darkred')

# A higher alcohol content appears to indicate a higher quality wine
plot(quality_red, red$alcohol, col='darkred')

# NOte that R-squared is .3606, adjusted .3561
# TODO: INTERPRET RESULT OF R-SQUARED
# Recall that the R-squared statistic allows us to measure how our model performed versus had we not used a model at all.
# The R-squared statistic of a linear model ... implies that/means 
# to examine coefficients
coef(lm.fit)
# confidence intervals -- by default this is 95% CI
ci=confint(lm.fit)
ci

# TODO: Interpret what these numbers say
# Recall that F-statistic tests overall significance -- tests whether relationship is statistically significant

# now let's find some outliers
studentized_resi = studres(lm.fit)
head(studentized_resi)
plot(studentized_resi)
outlier_indices=which(abs(studentized_resi)>3)
outlier_indices
# so our problematic outlier rows are:
# 46, 391, 441, 460, 653, 814, 833, 900, 1236, 1277, 1479, 1506

# and high leverage points
# this is calculated using cook's distance
# calculate cd for every data point
cd=cooks.distance(lm.fit)
# it is time-consuming to print, so we will settle for printing only the head()
head(cd)
# extract points where cooks distance is > 4/n for high leverage points

# how many observations in data?
n=nrow(red)
n
# 1599 observations
leverage_indices=which(cd>4/n)
leverage_indices
# there are quite a few here, so let us compare which overlap with our outliers
# outlier *and* high leverage points are:
# 46, 391, 441, 460, 653, 814, 833, 1236, 1277, 1479, 1506
# all outliers are high leverage points but not all high leverage points are outliers
# this is consistent with what we've learned about cooks distance


# we can combine these indices and remove both quite easily
remove_indices=union(outlier_indices,leverage_indices)
remove_indices

# check for high collinearity
# this can be done using vif(), in the car package
vif(lm.fit)

# vif is larger than 5 for fixed acidity and density which is problematic
# and may indicate collinearity issues
# consider removing these predictors

# TODO: REMOVE DENSITY & FIXED ACIDITY
# create model without fixed acidity and density due to high VIF
lm.fit=lm(quality~`volatile acidity`+`citric acid`+`residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+pH+sulphates+alcohol, data=red)
lm.fit
summary(lm.fit)
plot(lm.fit)

# and once more, with only significant variables
lm.fit=lm(quality~`volatile acidity`+chlorides+`total sulfur dioxide`+pH+sulphates+alcohol, data=red)
lm.fit
summary(lm.fit)
plot(lm.fit)
ci=confint(lm.fit)
ci

new_red=red[-remove_indices,]
# TODO: Change the ~. here to reflect changes or just move these lines of code up
lm.fit3=lm(quality~.,data=new_red)
summary(lm.fit3)

# RERUN MODEL AFTER REMOVING THOSE HIGH VIF VARIABLES


# now we can reexamine significance
# significant factors after removing outliers and high leverage points are:
# volatile acidity
# chlorides
# total sulfur dioxide
# sulphates
# alcohol

# these have remained the same
plot(lm.fit3)

# it looks more reasonable but NOTE THE FUNNEL SHAPE IN THE RESIDUALS VS LEVERAGE PLOT
# This may indicate heteroskedasticity?

# TODO: Let us remove the collinear predictors and try to improve the model.

plot(lm.fit3$fitted.values,lm.fit3$residuals)

# TODO: REMOVE THESE PREDICTORS AND RERUN?

# what if we examined it as a logarithmic model?
logfit=lm(log(quality)~., data=new_red)
# still maintains most significant variables
summary(logfit)
plot(logfit)
plot(logfit$fitted.values,logfit$residuals)
# this is a PROBLEM, there is no random distribution here whatsoever


# create polynomial with significant factors squared
newfit <- lm(quality ~ poly(alcohol,2) + poly(`volatile acidity`,2) + `residual sugar` + poly(`free sulfur dioxide`,2) + chlorides + sulphates + poly(pH,2), data=new_red)
summary(newfit)
plot(newfit)



# LASSO
library(glmnet)
x <- model.matrix(quality~., new_red)[,-1]
y <- new_red$quality
lasso <- cv.glmnet(as.matrix(x), y, alpha=1)

plot(lasso)

# examine coefficients with minimum CV errors
as.matrix(coef(lasso, lasso$lambda.min))
# examine coefficients with largest lambda value within 1 standard error of min
as.matrix(coef(lasso, lasso$lambda.1se))


k = 5 ##number of folds
n = length(y) ##number of observations
reddat = data.frame(y=y, x=x) ##set data in data frame
ncv = ceiling(n/k) ##calculate no. of obs. per fold
cv.ind.f = rep(seq(1:k),ncv) ##assign fold indices
cv.ind = cv.ind.f[1:n] ##restrict to only n observations
cv.ind.random = sample(cv.ind,n,replace=F)## choose fold indices randomly
#Split data into testing and training and compute cross validation error:
MSE = c(); cv.err = c(); nlam=length(L)
for (i in 1:nlam){for (j in 1:k){
  train = reddat[cv.ind.random!=j,]; response = train$y
  design = train[,names(reddat)!="y"]
  lasso.mod = glmnet(as.matrix(design),response,lambda=L[i])
# Compute MSE on testing set
  test = reddat[cv.ind.random==j,]; resp.values = test$y
  hb = coef(lasso.mod, s = L[i])
  fitted.values = (cbind(1,as.matrix(test[,names(reddat)!="y"])))%*%hb
  MSE[j] = mean((resp.values - fitted.values)^2)}
  cv.err[i] = mean(MSE)}

# print all cv errors
cv.err


min.ind=which.min(cv.err)##index of minimum cv err
lmin=L[min.ind]##lambda value at which minimum is attained
hb.best=coef(lasso.mod,s=lmin)##extract best fitting model
hb.best
hb.best[1:10]

# 5.646717 is best lambda value ???


```


## White Wine Quality
```{r white}
# import data for white wine
#library(readr)
white <- read_delim("winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(white)
summary(white)
lm.fit2=lm(quality~., data=white)
lm.fit2
summary(lm.fit2)

# for white wine, the most significant variables are
# volatile acidity
# residual sugar
# free sulfur dioxide
# density
# pH
# sulfates
# alcohol
quality_white = white$quality

plot(quality_white, white$`volatile acidity`)
plot(quality_white, white$`residual sugar`)
plot(quality_white, white$`free sulfur dioxide`)
plot(quality_white, white$`density`)
plot(quality_white, white$`pH`)
plot(quality_white, white$sulphates)
plot(quality_white, white$alcohol)

names(lm.fit2)
# to examine coefficients
coef(lm.fit2)
# confidence intervals -- by default this is 95% CI
ci2=confint(lm.fit2)
ci2

# IDENTIFY AND EXAMINE OUTLIERS AND HIGH LEVERAGE POINTS
studentized_resi2 = studres(lm.fit2)
head(studentized_resi2)
plot(studentized_resi2)

outlier_indices2=which(abs(studentized_resi2)>3)
outlier_indices2

# calculate cd for every data point
cd2=cooks.distance(lm.fit2)
# it is time-consuming to print, so we will settle for printing only the head()
head(cd2)
# extract points where cooks distance is > 4/n for high leverage points

# how many observations in data?
n2=nrow(red)
n2
# 1599 observations
leverage_indices2=which(cd>4/n)
leverage_indices2
# there are quite a few here, so let us compare which overlap with our outliers
# outlier *and* high leverage points are:
# 46, 391, 441, 460, 653, 814, 833, 1236, 1277, 1479, 1506
# all outliers are high leverage points but not all high leverage points are outliers
# this is consistent with what we've learned about cooks distance


# we can combine these indices and remove both quite easily
remove_indices2=union(outlier_indices2,leverage_indices2)
remove_indices2

# check for high collinearity
# this can be done using vif(), in the car package
vif(lm.fit2)

# vif is larger than 5 for fixed acidity and density which is problematic
# and may indicate collinearity issues
# consider removing these predictors

new_white=white[-remove_indices,]
lm.fit2=lm(quality~.,data=new_white)
summary(lm.fit2)


# suppose we want to find the best fitting model


# TODO: Can we identify what makes a white wine quality 10?
# TODO: Can we identify what makes a white wine quality 1?

```
