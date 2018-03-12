####### HW6_q1 #####

## a ##
# find the optimal c
# find most optimal theta
xtheta <- seq(0,3,by=0.01)
ytheta <- sqrt(2/(pi*xtheta^2))*exp((xtheta^2)/2)
par(mar=c(5,6,2,1))
plot(xtheta, ytheta, type="l", lwd=3, xlab=expression(theta), 
     ylab=expression(c[theta]), ylim=c(0,10), cex.lab=2) 
opttheta <- function(xtheta){sqrt(2/(pi*xtheta^2))*exp((xtheta^2)/2)}
opt_theta <- nlminb(0.001, opttheta)$par
c_opt <- round(nlminb(0.0001, opttheta)$objective, 3)

## b ##

#### inversion method ####
laplace <- function(x){
  if(x < 0){
    0.5*exp(x)
  } else {
    0.5*exp(-x)
  }
}
quant_lap <- function(u){
  if (u < 1/2){
    log(2*u)
  } else {
    -log(2*(1-u))
  }
}

my_u <- runif(1000)
lap <- seq(-6,6,by=0.1)
lap.pdf <- sapply(lap, laplace)
rv_lap <- sapply(my_u, quant_lap)
par(mar=c(5,4,2,1))
library("MASS")
truehist(rv_lap, col="gray70", ylim=c(0,0.5), nbins=100,
         xlab="Random Variables")
lines(density(rv_lap)$x,density(rv_lap)$y, col="red", lwd=3)
lines(lap, lap.pdf, col="blue", lwd=3)
legend("topright", c("Empirical Density", "Theoretical Density"), col=c("red", "blue"), 
       lwd=3, bty="n")

### rejection method ####

goverf = function(x,theta=opt_theta){
  y = (sqrt(2*pi)*theta/2)*exp(x^2/2 - theta*abs(x))
  return(y)}

xc = rv_lap # generated 1000 Laplace RVs
uc=runif(1000,0,1)
tc = c_opt*sapply(xc, goverf) #c_opt as determined in the first part of the problem
ut = uc*tc
acc_obs <- xc[ut <=1]
#plot #
length(acc_obs)/1000
par(mar=c(5,3,2,1))
truehist(acc_obs, col="gray70", ylim=c(0,0.6), nbins=100,
         xlab="Accepted Random Variables")
lines(density(acc_obs)$x,density(acc_obs)$y, col="red", lwd=3)
lines(seq(-4,6,by=0.1), dnorm(seq(-4,6,by=0.1)), col="blue", lwd=3)
legend("topright", c("Empirical Density", "Theoretical Density N(0,1)"), col=c("red", "blue"), 
       lwd=3, bty="n")
sum(ut<=1)/1000

### c ####

ks.test(acc_obs, "pnorm", 0,1) #$statistic # reject null hyp (it is not coming from a N(0,1))
