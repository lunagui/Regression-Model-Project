---
title: "Using Regression Models Explor Motor Trend"
author: "Luna Gui"
date: "23 January 2015"
output: html_document
---

## Summary
We work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:  
  
- “Is an automatic or manual transmission better for MPG”  
- "Quantify the MPG difference between automatic and manual transmissions"  

## Load and Explor Data  
First, we load the mtcars data and perform some basic exploratory data analyses.
```{r}
library(UsingR)
data(mtcars)
str(mtcars)
colSums(is.na(mtcars))
```
  
We get the basic imformation of data. If we treat mpg as our outcome, we have 10 variables may influnce this outcome. We intersted in automatic or manual transmission which is the am variable.  
```{r}
library(ggplot2)
mtcars$am <- factor(as.character(mtcars$am), levels = c(0,1), labels = c("automatic", "manual"))
g <-ggplot(mtcars, aes(x = am, y=mpg, fill = am)) 
g  + geom_violin(col = "black", size = 2) + labs(title = "mpg by two transmission")
```
  
**It looks manual transmission better than automatic transmission.**  

```{r}
fit <- lm(mpg ~ am, mtcars)
summary(fit)
```
The result indicates the averge mpg of manual transmission is 7.24 higher than that of automatic transmission (17.15 as intercept) with a p-value less than 0.1% significance. The adjusted R2 value(0.3385) indicates that the model explains only 34% of the variations. It’s a very low value.
  
## Model selection  

```{r}
fitall <- lm(mpg ~ ., mtcars)
summary(fitall)
```
If we fit all variable to regression model, The adjusted R2 value is good. But we have higher p value for almost all variable.

```{r}
pairs(mtcars)
```
  
The pairs plot shows that lots of variable have coraltion.   
  
We will try the Stepwise Algorithm  to select a better model but keeping am variable in the model:

```{r}
fit.step <- step(lm(mpg~ ., mtcars), trace=0, scope=list(lower=~am), direction="both")
summary(fit.step)
```
  
This model looks better. But we should check the cor between them.  
```{r}
par(mfrow = c(1,2))
plot(mtcars$am, mtcars$wt, ylab = "wt")
plot(mtcars$am, mtcars$qsec, ylab = "qsec")
fitcor <- lm(mpg~ wt+am+qsec+wt*am, mtcars)
anova(fit, fit.step, fitcor)
summary(fitcor)$adj.r.squared
```
We change the model a bit based on coralation between am and wt.  
The p-value is very low: we can then reject the null hypothesis (i.e. “Model are equals”) and claim that the model fitcor is best one. And the adjusted R2 value(0.8804219) indicates that the model explains 88% of the variations. It's quite good.  
  
We check the modle's Residual Analysis:
```{r}
par(mfrow = c(2,2))
plot(fitcor)
```
  
  The above figure is a residual plot of the selected model. Residuals seems to be uncorrelated with the fit, independent and (almost) identically distributed with mean zero.
  
## Result
```{r}
summary(fit.step)
```
Given the coefficients of our model, we can see manual cars have more effectient than automatic cars: they have mor 2.94 miles per galon (MPG) performance than automatic cars. This value can be obtained when we consider the weight (wt) and the “1/4 mile time” (qsec) variables of the cars of our dataset.

```{r}
summary(fitcor)
```
If we consider the coralation between am and wt(This modle meaures coeficient of wt*am, and it is significant.). We can say manual cars look have supper effectient than automatic cars, but manual cars get less performance when wt increase. The manual cars will decrease more(-4.141) per wt unit(lb/1000) than automatic cars.(If you pursue lower fuel consumption, please choose light manual car. )