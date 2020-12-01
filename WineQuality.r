---
title: "Wine Quality"
author: "First Last"
date: "11/27/2020"
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

quality_red = red$quality

# It appears that the most significant variables are
# volatile acidity
# chlorides
# total sulfur dioxide
# sulphates
# alcohol

plot(quality_red, red$`volatile acidity`, col='darkblue')
plot(quality_red, red$chlorides)
plot(quality_red, red$`total sulfur dioxide`)
plot(quality_red, red$sulphates)

# A higher alcohol content appears to indicate a higher quality wine
plot(quality_red, red$alcohol)

# TODO: GET OUT OLD NOTES AND EXAMINE THESE RESULTS

# look at all variables in lm.fit object
names(lm.fit)
# to examine coefficients
coef(lm.fit)
# confidence intervals -- by default this is 95% CI
ci=confint(lm.fit)
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

# TODO: IDENTIFY AND EXAMINE OUTLIERS AND HIGH LEVERAGE POINTS
# TODO: Can we identify what makes a white wine quality 10?
# TODO: Can we identify what makes a white wine quality 1?

```

