#### HW6_q3 ####

fx <- function(x){
  if(-1 <= x & x <= 0){
    x+1
  }else if(0 <= x & x <= 1){
    -x+1
  }else{0}
}
xi=runif(1000, -2,2)
xi <- xi[order(xi)]
plot.fx <- sapply(xi, fx)
par(mar=c(5,5,2,2))
plot(xi, plot.fx, type="l", lwd=3, ylab="f(x)")

#lines(xi, 2*dunif(xi, -1,1))

#### gen rej ####
fog  <- function(x){      ## f(x) over g(x)
  if(-1 <= x & x <= 0){
    2*x+2
  }else if(0 <= x & x <= 1){
    -2*x+2
  }else{0}
}
plot.fog <- sapply(xi, fog)
par(mar=c(5,6.5,2,2))
plot(xi, plot.fog, type="l", lwd=3, ylab=expression(paste("c = ", frac(f(x), g(x)))))
c_opt <- optimize(fog, c(-3,3), maximum = T)$objective


gof  <- function(x){      ## g(x) over f(x)
  if(-1 <= x & x <= 0){
    1/(2*x+2)
  }else if(0 <= x & x <= 1){
    1/(-2*x+2)
  }else{0}
}

xc = runif(1000, -1,1) # generated 1000 Laplace RVs
uc=runif(1000,0,1)
tc = c_opt*sapply(xc, gof) #c_opt as determined in the first part of the problem
ut = uc*tc
acc_obs <- xc[ut <=1]
#plot #
length(acc_obs)/1000
par(mar=c(5,3,2,1))
truehist(acc_obs, col="gray70", ylim=c(0,1),nbins=10,
         xlab="Accepted Random Variables", xlim=c(-1.5,1.5))
lines(density(acc_obs)$x,density(acc_obs)$y, col="red", lwd=3)
lines(xi, plot.fx, col="blue", lwd=3)
legend("topright", c("Emprical Density", "Theoretical Density"), col=c("red", "blue"), 
       lwd=3)
sum(ut<=1)/1000


#### simple rej ####

c_rej <- optimize(fx, c(-1,1), maximum=T)$objective

xc = runif(200, -1,1) # generated 1000 Laplace RVs
uc=runif(200,0,1)
fxi = sapply(xc, fx)
tc = c_rej/fxi #c_opt as determined in the first part of the problem
ut = uc*tc
acc_obs <- xc[ut <=1]
length(acc_obs)/200


#### inversion ####

f_inv <- function(x){
  if(0 < x & x < 0.5){
    sqrt(2*x)-1
  }else if(0.5 <= x & x < 1){
    1 - sqrt(2*(-x+1))
  }else{0}
}

U <- runif(100)
RVs <- sapply(U, f_inv)

par(mar=c(5,3,3,1), mfrow=c(1,2))
truehist(acc_obs, col="gray70",nbins=10, main="Rejection Method",
         xlab="Accepted Random Variables",ylim=c(0,1.1), xlim=c(-1.5,1.5))
lines(density(acc_obs)$x,density(acc_obs)$y, col="red", lwd=3)
lines(xi, plot.fx, col="blue", lwd=3)
 legend(0.9, 1.25, c("Empirical Density", "Theoretical Density"), col=c("red", "blue"), 
         lwd=3,xpd=NA,bty = "n", cex=1.2)
truehist(RVs, col="gray70",nbins=10, main="Inversion Method",
         xlab="Accepted Random Variables",ylim=c(0,1.1), xlim=c(-1.5,1.5))
lines(density(RVs)$x,density(RVs)$y, col="red", lwd=3)
lines(xi, plot.fx, col="blue", lwd=3)

#dev.off()

