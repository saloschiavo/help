---
title: "Wine Quality"
author: "Stefanie LoSchiavo"
date: "11/29/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Red Wine Quality
```{r red}
# import data for red wine
library(readr)
red <- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(red)
summary(red)

# using multiple linear regression to start
lm.fit=lm(quality~., data=red)
lm.fit
summary(lm.fit)

# It appears that the most significant variables are
# volatile acidity
# chlorides
# total sulfur dioxide
# sulphates
# alcohol

plot(quality,`volatile acidity`)
plot(quality,chlorides)
plot(quality,`total sulfur dioxide`)
plot(quality,sulphates)

# A higher alcohol content appears to indicate a higher quality wine
plot(quality,alcohol)

# TODO: GET OUT OLD NOTES AND EXAMINE THESE RESULTS

# note the coefficients or least squares estimates in the first column, the SE, the t-stat, and the p-values
# based on this p-value, the chances that B0 is not actually 0 is very small

# look at all variables in lm.fit object
names(lm.fit)
# to examine coefficients
coef(lm.fit)
# confidence intervals -- by default this is 95% CI
ci
# TODO: Interpret what these numbers say


# TODO: IDENTIFY AND EXAMINE OUTLIERS AND HIGH LEVERAGE POINTS
# TODO: Can we identify what makes a red wine quality 10?
# TODO: Can we identify what makes a red wine quality 1?


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
plot(quality, `volatile acidity`)
plot(quality, `residual sugar`)
plot(quality, `free sulfur dioxide`)
plot(quality, `density`)
plot(quality, `pH`)
plot(quality, sulphates)
plot(quality, alcohol)


# TODO: IDENTIFY AND EXAMINE OUTLIERS AND HIGH LEVERAGE POINTS
# TODO: Can we identify what makes a white wine quality 10?
# TODO: Can we identify what makes a white wine quality 1?

```

