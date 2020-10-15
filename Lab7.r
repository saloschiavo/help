##STAT 435: High Dimensional Regression, lasso, and ridge regression
# Lab 7

## simulate data with more parameters than observations
# to make this feasible, we must assume its sparse (only 5 are nonzero)

# n represents sample size
# p is dimension (number of parameters)
# s: sparsity
n=200; p=300; s=5;
x=matrix(rnorm(n*p),n,p)
# repeat function to make first 5 non-zero
b=c(rep(1,s),rep(0,p-s))
y=1+x%*%b+rnorm(n)


### from here on, we assume that we only have access to x,y (only dataset)
# objective: recover the coefficients 'b'

# let's start with what doesn't work!
# try  OLS (ordinary least squares) using lm()
mod=lm(y~x)
coef(mod)

#look at these NA values, this isn't great and the other ones might not be representative of the truth
# these coefficients should be mostly 0, right?
# we observe a large number of NA's
# this is because ordinary least squares is not valid here because of high dimensions

## solution: lasso or ridge regression
## these can be implemented by the r-package 'glmnet'
# install and unpack glmnet
# install.packages('glmnet')
library(glmnet)

# we can now use glmnet() function to implement lasso and ridge regression
?glmnet # for more information

# recall what we need to implement lasso: lambda! that's in this package.

## set a grid of values for lambda
lam=seq(0.001,1,length.out = 100)
lam

# we want to run a regression for each value of lambda

## implement lasso -- (design matrix, response, lambda values with alpha=1 parameter)
# alpha=1 indicates that we want to compute lasso estimator
# if we omit the alpha=1, the function will default to lasso
lasso.mod=glmnet(x,y,lambda=lam,alpha = 1)
# extract coefficients at any particular value of lambda
hbeta=coef(lasso.mod,s=0.2) ##we are extracting at lambda=0.2
# the s in this function represents lambda, it has nothing to do with previous s for sparsity
as.numeric(hbeta)
# note the 300 coefficients that this prints
# variable selection: most coefficients have been set to 0
# except the TRUE ones-- the first one 0.959 represents the intercept

# if we change lambda, the solution will change
hbeta=coef(lasso.mod,s=0.1)
as.numeric(hbeta)

# so which one should we actually use? which is the best solution?
# choosing the best lambda can be done via cross validation
# but for now, we don't have to worry about this! we might do it in next lab
# instead we will attempt it in our quiz using cross validation in lab 7

# ridge regression can be done in a similar manner
ridge.mod=glmnet(x,y,lambda=lam,alpha = 0)
# alpha = 0 indicates ridge penalty
hbeta=coef(ridge.mod,s=0.2)
as.numeric(hbeta)
# variable selection: ridge regression no longer has variable selection property

# as before the best value of lambda can be chosen by cross validation
# which, as of now, we will not be looking at yet
