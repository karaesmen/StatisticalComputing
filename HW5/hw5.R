setwd("./Google Drive/Statistical Computing/HW5/")
##### HW5_1a ####
sim <- 10000
n=20
rej <- NULL
xi <- rnorm(sim, 0.5, 1)

for(i in 1:sim){
  xsam <- sample(xi, n, replace=T)
  xbar <- mean(xsam)
  rej[i] <- (abs(xbar/(1/sqrt(n))) > 1.96)*1
}

sum(rej)/sim

###### HW5_1b ####
xi <- rexp(n, rate=(1/2*(1/20)^2))
gxi <- 1 - ( (2*(1/20)/(sqrt(2*pi)))* exp( (2*xi-xi^2)/2*(1/20)^2 ) )
sum(gxi)/n
##### HW5_1b #####
mu1 <- 0.5
mu2 <- 0
sd1 <- 1/(n)
sd2 <- 1/(n)

xs <- seq(min(mu1 - 3*sd1, mu2 - 3*sd2), max(mu1 + 3*sd1, mu2 + 3*sd2), .01)
f1 <- dnorm(xs, mean=mu1, sd=sd1)
f2 <- dnorm(xs, mean=mu2, sd=sd2)

pdf(".\AUC.pdf")
par(mar=c(5,5,2,1))
plot(xs, f1, type="l", ylim=c(0, max(f1,f2)), ylab="Density",xlab = "X", 
     cex.lab=1.5,lwd=3, col="black")
lines(xs, f2, lwd=3, col="red")
legend("top", 2, c("X ~ N(0,1/n)", "X ~ N(0.5,1/n)"), lty=1, lwd=c(4,4),
       col=c('red', 'black'),  cex=1.2)

### sample means

mu1 <- 0.5
mu2 <- 0
sd1 <- 1/(n)
sd2 <- 1/(n)

xs <- seq(min(mu1 - 3*sd1, mu2 - 3*sd2)-2, max(mu1 + 3*sd1, mu2 + 3*sd2)*3, .01)
f1 <- dnorm(xs, mean=mu1, sd=sd1)
f2 <- dnorm(xs, mean=mu2, sd=sd2)

par(mar=c(5,5,2,1))
plot(xs, f1, type="l", ylim=c(0, max(f1,f2)), ylab="Density",xlab = expression(bar(x)), 
     cex.lab=1.5,lwd=3, col="black")
lines(xs, f2, lwd=3, col="red")
legend("top", 2, c("X ~ N(0,1/(n=20))", "X ~ N(0.5,1/(n=20))"), lty=1, lwd=c(4,4),
       col=c('red', 'black'),  cex=1)


plot(xs, f1, type="l", ylim=c(0, max(f1,f2)),xlim=c(-0.5,1.5), ylab="X ~ N(0.5,1/(n=20))",xlab = expression(bar(x)), 
     cex.lab=1.5,lwd=3, col="black")

##### HW5_3 ####

n <- 25
set.seed(938)
xi <- exp(rnorm(n,0,1))
mu<- mean(xi)
s <- sqrt(var(xi))
s_hat <- sum((xi - mu)^3/s^3)/n # estimated skewness value

nsim1 <- 1000
nsim2 <- 100
s_err <- NULL
Tboot <- NULL


for(i in 1:nsim2){ # repeating bootstrapping
  for(j in 1:nsim1){ # bootstrapping
    boot <- sample(xi, 25, replace=T)
    skew <- (boot - mu)^3/s^3
    Tboot[j] <- mean(skew)
  }
s_err[i] <- sqrt(var(Tboot))
}

ci1 <- s_hat - s_err*1.96
ci2 <- s_hat + s_err*1.96
CIs <- data.frame(ci1,ci2)
real_skew <- (exp(1)+2)*sqrt(exp(1)-1)
Tval <- cbind( CIs[,1] <= real_skew & CIs[,2] >= real_skew )*1 # set columns of the matrix as intervals 
sum(Tval)
sum(Tval)/length(Tval) * 100 # % of CI contains true value
100*sum(cbind( CIs[,1] <= s_hat & CIs[,2] >= s_hat )*1)/nsim2
