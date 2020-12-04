---
title: "Wine Quality"
author: "First Last"
date: "11/27/2020-12/4/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Our analysis is based on the Wine Quality data sets (available from the UCI Machine Learning Repository), for both red and white wines.
The red wine data set contains 12 variables, and n=1599.
The white wine data set contains 12 variables, and n=4898.
We set out to see if we could determine how the various chemical levels in red and white wines might affect their quality.
Throughout the project, we explored many different potential models, each of which is outlined and examined here.


Source:
Paulo Cortez, University of Minho, Guimarães, Portugal, http://www3.dsi.uminho.pt/pcortez
A. Cerdeira, F. Almeida, T. Matos and J. Reis, Viticulture Commission of the Vinho Verde Region(CVRVV), Porto, Portugal
@2009
Data downloaded from https://archive.ics.uci.edu/ml/datasets/Wine+Quality


## Red Wine Quality
```{r red}
# Install required packages
library(MASS)
library(car)
library(readr)
# Import data for red wine from csv file using ; as delimiter
red <- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(red)
summary(red)

# Use multiple linear regression model to start
lm.fit=lm(quality~., data=red)
lm.fit
# Look at all variable names in lm.fit object
names(lm.fit)
# Summarize
summary(lm.fit)
# Note that small p-value indicates significance of multiple independent variables

# Plot the model itself
plot(lm.fit)

# Assign quality into its own variable
quality_red = red$quality

# Look at distribution of quality of red wine throughout data set
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
plot(quality_red, red$alcohol, col='darkred')

# Note that in the cases of alcohol and volatile acidity vs quality, red wines' alcohol level between 9-12%,
# the level of volatile acidity decreases as alcohol level increases.

# Alcohol and sulphates both have positive relationships with quality (they both increase together).
# Note that as volatile acidity and total sulfur dioxide increase, the quality tends to decrease.
# High levels of acetic acid tend to lower the quality of red wine as well.

# Recall that the R-squared statistic allows us to measure how our model performed versus had we not used a model at all.
# Note that R-squared is .3606, adjusted .3561, which means that our current model is not a very good fit (only roughly
# a third of the data fits onto the model).

# Let us examine coefficients
coef(lm.fit)
# Calculate confidence intervals -- by default, this is 95% CI
ci=confint(lm.fit)
ci

# Identify outliers using studentized residuals
studentized_resi = studres(lm.fit)
head(studentized_resi)
plot(studentized_resi)
# Identify indices of outliers
outlier_indices=which(abs(studentized_resi)>3)
outlier_indices
# So our problematic outlier rows are:
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

# Rebuild multiple regression model without outliers-- THIS IS OUR BEST MODEL
# This appears to be the best model out of all multiple linear regression models
lm.fit3=lm(quality~.,data=new_red)
summary(lm.fit3)

# Now let us reexamine significance.
# The significant factors after removing outliers and high leverage points are:
# volatile acidity
# chlorides
# total sulfur dioxide
# sulphates
# alcohol

plot(lm.fit3)
# This plot looks more reasonable, but note the funnel shape
# in the residuals vs leverage plot. This may be an indication of heteroskedasticity.
# As a result, let us remove the collinear predictors causing the issue
# in an attempt to improve the model.

# Check for high collinearity with vif(), featured in the car package
vif(lm.fit3)

# Note that the variance inflation factors (vif) are larger than 5 for fixed acidity and density,
# which is indicative of a problem and may indicate collinearity issues.
# As a result, these predictors should be removed from the model.

# Create a new multiple linear regression model without fixed acidity and density due to high VIF
# This was created from the dataset with outliers and high leverage points removed
lm.fit=lm(quality~`volatile acidity`+`citric acid`+`residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+pH+sulphates+alcohol, data=new_red)
lm.fit
summary(lm.fit)
plot(lm.fit)
ci=confint(lm.fit)
ci
# This model is slightly less accurate than the above model.

# Create another linear model for comparison, this time using only significant variables
lm.fit=lm(quality~`volatile acidity`+chlorides+`total sulfur dioxide`+pH+sulphates+alcohol, data=red)
lm.fit
summary(lm.fit)
plot(lm.fit)
ci=confint(lm.fit)
ci
# The previous model without fixed acidity and density performs slightly better
# than this model with only significant values present.

# Exploring the option of a logarithmic model:
logfit=lm(log(quality)~., data=new_red)
# still maintains most significant variables
summary(logfit)
plot(logfit)
plot(logfit$fitted.values,logfit$residuals)
# This still appears problematic, as there is minimal random distribution present,
# however, the residual standard error has been greatly reduced.

# Create polynomial with significant factors squared
newfit <- lm(quality ~ poly(alcohol,2) + poly(`volatile acidity`,2) + `residual sugar` + poly(`free sulfur dioxide`,2) + chlorides + sulphates + poly(pH,2), data=new_red)
summary(newfit)
plot(newfit)
# This appears to be one of the better models, as the residual standard error is 0.5622,
# but it is still not the best fit.

# LASSO
set.seed(1)
library(glmnet)
x <- model.matrix(quality~., new_red)[,-1]
y <- new_red$quality
lasso <- cv.glmnet(as.matrix(x), y, alpha=1)
plot(lasso)

# examine coefficients with minimum CV errors
as.matrix(coef(lasso, lasso$lambda.min))
# examine coefficients with largest lambda value within 1 standard error of min
as.matrix(coef(lasso, lasso$lambda.1se))

# define grid of values for regularizer
L=seq(0,1,length.out = 100)

# compute mean squared error
mse = c() # initialize empty vector for mse values
# compute mse for each value of lambda
for (i in 1:length(L)) {hb = coef(lasso,L[i])
y_hat = cbind(1,x)%*%hb; mse[i] = mean((y_hat - y)^2)}
# The least value of mse is obtained at the smallest value of λ when error is computed on the training set
plot(L,mse,type="l")

# Compute all cross validation errors
# Set number of folds
k = 5
# n = number of observations
n = length(y)
# reddat is our data frame for red wine observations and values
reddat = data.frame(y=y, x=x)
# Calculate number of observations per fold
ncv = ceiling(n/k)
# Assign indices to the folds
cv.ind.f = rep(seq(1:k),ncv)
# Restrict to only n observations
cv.ind = cv.ind.f[1:n]
# this chooses the indices randomly
cv.ind.random = sample(cv.ind,n,replace=F)
# Split data into testing and training to compute cross validation error:
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

# Print all cv errors
cv.err

# Identify index of minimum cross validation error
min.ind=which.min(cv.err)
# Lambda value where the minimum error is observed
lmin=L[min.ind]
# Extract the best fitting model
hb.best=coef(lasso.mod,s=lmin)
hb.best
hb.best[1:10]

# 5.64 is best lambda value found

# Plot lambda vs cross validation error
plot(L,cv.err,type="l")


```


## White Wine Quality
```{r white}
# Import data for white wine
library(readr)
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

# TODO: Describe the relationships involved here -- if you could do this,
# if you happen to have spare time, that would be amazing! This is really
# the last thing we have left to do, I believe!

names(lm.fit2)
# to examine coefficients
coef(lm.fit2)
# confidence intervals -- by default this is 95% CI
ci2=confint(lm.fit2)
ci2

# Identify and examine outliers and high leverage points
studentized_resi2 = studres(lm.fit2)
head(studentized_resi2)
plot(studentized_resi2)

outlier_indices2=which(abs(studentized_resi2)>3)
outlier_indices2

# Calculate cd for every data point
cd2=cooks.distance(lm.fit2)
# It is time-consuming to print, so we will settle for printing only the head()
head(cd2)
# Extract points where cooks distance is > 4/n for high leverage points

# How many observations in data?
n2=nrow(white)
n2
# 4898 observations
leverage_indices2=which(cd>4/n)
# There are quite a few here, so let us compare which overlap with our outliers
# Outlier *and* high leverage points are:
leverage_indices2
# we can combine these indices and remove both quite easily
remove_indices2=union(outlier_indices2,leverage_indices2)
remove_indices2
new_white=white[-remove_indices,]

# Check for high collinearity, again using vif() function
vif(lm.fit2)

# vif is larger than 5 for fixed acidity and density which is problematic
# and may indicate collinearity issues
# consider removing these predictors
lm.fit2=lm(quality~`volatile acidity`+`free sulfur dioxide`+`pH`+`sulphates`,data=new_white)
summary(lm.fit2)
plot(lm.fit2)

# Note that the R-squared statistic here still indicates a fairly weak model (just under half of
# the provided white wine data fits onto the model). However, the standard error is quite high.

# TODO: If you could revise this/elaborate on the analysis, that would be fantastic.

```
## CONTRIBUTIONS FROM EACH GROUP MEMBER

S was responsible for removing outliers and high leverage points, examining vif values,
building the logarithmic models, as well as the LASSO model, and computing cross validation.
A was responsible for building several of the multiple linear regression models. He interpreted the results of the white wine.
Both of us shared our work and thoroughly discussed our results, forming an analysis together.
