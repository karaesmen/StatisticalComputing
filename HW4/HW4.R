##### HW4 ############

setwd("./Google Drive/Statistical Computing/HW4")


#### HW4_1 ####

obs <- c(rep.int(0, 7840), rep.int(1, 1327), rep.int(2, 239), rep.int(3, 42), 
         rep.int(4, 14), rep.int(5,4), rep.int(6,4), rep.int(7,1))
hist(obs, probability = T, breaks=seq.int(0, 7, by=1))

poisson.lik<-function(lambda){
  n<-length(obs)
  logl<-sum(obs)*log(lambda)-n*lambda
  return(logl)
}

pos.lam <- seq(0, 3, by=0.001)[-1]
poisson.lik2 <- function(lambda){-1*sapply(lambda, poisson.lik)}
l.pois <- poisson.lik2(lambda = pos.lam)
library(ggplot2)
qplot(pos.lam, -l.pois, xlab = expression(paste("Values of ", lambda)), ylab = "Log-likelihood", 
     type="l", main=expression(paste("Log-likelihood function of Poisson(", lambda, ")")))

mle_lamb <- nlminb(3, poisson.lik2)$par # 3 is the start value for optimization
min(l.pois)

ppois(2, lambda=mle_lamb)
g_lamb <- function(x, lambda){lambda^x*exp(-lambda)/factorial(x)}
g_lamb(2, mle_lamb)

#### HW4_3 #####


Xs <- c(2.8,5.6,24.7,6.5,1.6,10.6,1.0,7.8,7.2,13.9)
Ys <- c(0,0,0,1,1,1,0,0,1,0,0,0,0,0,0)
  
joint.lik<-function(theta){
  n<-length(Xs)
  m <- length(Ys)
  
  logl <- -n*log(theta, base = exp(1)) - sum(Xs)/theta - 5*sum(Ys)/theta + (m - sum(Ys))*log((1-exp(-5/theta)), base = exp(1))
  
  return(logl)
}

optimize(joint.lik, c(10,0), maximum=TRUE)

pos.t <- seq(0, 10, by=0.05)[-1]
library(ggplot2)
qplot(pos.t, joint.lik(pos.t), xlab = expression(paste("Values of ", lambda)), ylab = "Log-likelihood", 
      type="l", main=expression(paste("Log-likelihood function of Poisson(", lambda, ")")))
