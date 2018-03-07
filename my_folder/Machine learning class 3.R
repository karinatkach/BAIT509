library(tidyverse)
install.packages("knitr")
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

#q1 = 2.73

#q2
dat$d <- abs(dat$x-0)
dat.2 <- arrange(dat,d)[1:7, ]

mean.knn <- mean(dat.2$y)

#q3 

dat.3 <- filter(dat,d<=1)
mean.loess<-mean(dat.3$y)


###################
xgrid <- seq(-5, 4, length.out=1000)

kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  dat.2 <- arrange(dat,d)[1:7, ]
  yhat <- mean(dat.2$y)
  return(yhat)
})
kNN_estimates
plot (kNN_estimates~xgrid)


loess_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  dat.3 <- filter(dat,d<=1)
  mean.loess<-mean(dat.3$y)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y)) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()


#value of parameter is too high => underfitting the data (features of the data are not clearly present in the model) => take almost all the data => get mean in the prediction
# overfitting - model captures features that are not actually present in the data
# underfitting - model does not capture the features present in the data
# map double takes a vector as the first arg and a function as a second argument; then it applies the function to every number in the vector (and returns a vector)
