---
title: "Wine Quality"
author: "First Last"
date: "11/27/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Make some interesting comments about the data set.


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
summary(lm.fit)
# note that small p-value indicates significance of multiple independent variables

# plot the model itself
plot(lm.fit)

# assign quality into its own variable
quality_red = red$quality

# look at distribution of quality of red wine throughout dataset
summary(quality_red)

# It appears that the most significant variables are:
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

# TODO: INTERPRET RESULT OF R-SQUARED
# Recall that the R-squared statistic allows us to measure how our model performed versus had we not used a model at all.
# Note that R-squared is .3606, adjusted .3561
# The R-squared statistic of a linear model ... implies that/means 

# Let us examine coefficients
coef(lm.fit)
# Calculate confidence intervals -- by default, this is 95% CI
ci=confint(lm.fit)
ci

# TODO: Interpret what these numbers say
# I believe this is related to the relationship between variables and quality

# Recall that F-statistic tests overall significance -- tests whether relationship is statistically significant

# now let's find some outliers!
studentized_resi = studres(lm.fit)
head(studentized_resi)
plot(studentized_resi)
outlier_indices=which(abs(studentized_resi)>3)
outlier_indices
# so our problematic outlier rows are:
# 46, 391, 441, 460, 653, 814, 833, 900, 1236, 1277, 1479, 1506

# Now to identify high leverage points:
# Calculate Cook's Distance for every point
cd=cooks.distance(lm.fit)
# It is time-consuming to print, so we will settle for printing only the head()
head(cd)
# Extract points where cooks distance is > 4/n for high leverage points

# Check to see how many observations in data
n=nrow(red)
n
# n = 1599 observations
leverage_indices=which(cd>4/n)
leverage_indices
# There are quite a few high leverage points, so let us compare which overlap with our outliers
# Outlier *and* high leverage points are:
# 46, 391, 441, 460, 653, 814, 833, 1236, 1277, 1479, 1506
# Observe that all outliers are high leverage points, but not all high leverage points are outliers
# This is consistent with what we've learned about Cook's Distance.

# Now let us combine these indices from the outliers and high leverages
remove_indices=union(outlier_indices,leverage_indices)
# Examine indices
remove_indices
# Remove the union of both values
new_red=red[-remove_indices,]

# Check for high collinearity with vif(), featured in the car package
vif(lm.fit)

# Note that the variance inflation factors (vif) are larger than 5 for fixed acidity and density,
# which is indicative of a problem and may indicate collinearity issues.
# As a result, these predictors should be removed from the model.

# Create a new model without fixed acidity and density due to high VIF
lm.fit=lm(quality~`volatile acidity`+`citric acid`+`residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+pH+sulphates+alcohol, data=red)
lm.fit
summary(lm.fit)
plot(lm.fit)

# Create another model for comparison, this time using only significant variables
lm.fit=lm(quality~`volatile acidity`+chlorides+`total sulfur dioxide`+pH+sulphates+alcohol, data=red)
lm.fit
summary(lm.fit)
plot(lm.fit)
ci=confint(lm.fit)
ci

# TODO: Change the ~. here to reflect changes or just move these lines of code up
lm.fit3=lm(quality~.,data=new_red)
summary(lm.fit3)

# Now let us reexamine significance
# The significant factors after removing outliers and high leverage points are:
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
#import data for white wine
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
n2=nrow(white)
n2
# 4898 observations
leverage_indices2=which(cd>4/n)
# there are quite a few here, so let us compare which overlap with our outliers
# outlier *and* high leverage points are:
leverage_indices2
# we can combine these indices and remove both quite easily
remove_indices2=union(outlier_indices2,leverage_indices2)
remove_indices2
new_white=white[-remove_indices,]

# check for high collinearity
# this can be done using vif(), in the car package
vif(lm.fit2)

# vif is larger than 5 for fixed acidity and density which is problematic
# and may indicate collinearity issues
# consider removing these predictors

lm.fit2=lm(quality~`volatile acidity`+`free sulfur dioxide`+`pH`+`sulphates`,data=new_white)
summary(lm.fit2)



plot(quality_white, white$`volatile acidity`)
plot(quality_white, white$`free sulfur dioxide`)
plot(quality_white, white$`pH`)
plot(quality_white, white$sulphates)



```

