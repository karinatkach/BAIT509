#Karina Tkach 22575005 Lecture 1 exercises

library(tidyverse)
library(ISLR)
library(knitr)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

dat <- genreg(1000)

#modifying data frame and adding predictions
#we don't know any of the predictors => we are gonna predict the same thing every time => 0
dat <- mutate(dat,
       yhat = 5,
       yhat1=5-x1,
       yhat2=5+2*x2,
       yhat12=5-x1+2*x2)
head(dat)

#irreducible error decreases when we know different predictors as the variance decreases (i.e. when we don't know both x1-x2, the variance is 6, mean 0; when we know x2 variance drops to 2 etc.)

#calculating error

#mse when we don't know any predictors

mse <- mean((dat$yhat - dat$y)^2)

#when know x1, x2, x1 and x2
mse1 <- mean((dat$yhat1 - dat$y)^2)

mse2 <- mean((dat$yhat2 - dat$y)^2)

mse12 <- mean((dat$yhat12 - dat$y)^2)


mse
mse1
mse2
mse12

#mse is decreasing
#the more predictors we observe the better predictions we get 
#best to worst: using x1&x2, x2, x1, neither x1 or 2





##########################################
#Oracle classification exercise
##########################################

#function generating X ~ N(0,1); takes sample size as an argument 
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}

pA1 <- 0.2 
pB1 <- 0.8/(1+exp(-1))
pC1 <- 1-pA1-pB1


pA2 <- 0.2 
pB2 <- 0.8/(1+exp(2))
pC2 <- 1-pA2-pB2

#2 - Classify as A when for p we get between 0 and 0.2, as B when we get between 0.2 and 0.2+pB, and as C when we get between 0.2+pB and 1

#3 
data <- gencla(1000)

#4
data <- mutate(data,
               pA = 0.2,
              pB = 0.8/(1+exp(-data$x)),
              pC = 1-0.2-0.8/(1+exp(-data$x))
              )
head(data)

data <- mutate(data,
               yhat = sapply(x, function(x_)
                 if (x_<0) "C" else "B"))
head(data)
#5. Error rate
1-mean(data$yhat==data$y)
