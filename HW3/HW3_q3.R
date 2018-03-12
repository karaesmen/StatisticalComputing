########### HW3_3  ###############
################################

######### HW3_3a #################

fx <- function(x, a, b){(1/beta(a,b))*x^(a-1)*(1-x)^(b-1)}
var<-seq(0,1,length=1000)
as <- c(0.5, 1,0.5, 1, 2, 5)
bs <- c(0.5, 0.5, 1, 1, 2, 10)
plot(var, fx(x=var, a=as[1], b=bs[1]), type="l", lwd=4, xlab="x",
     ylab="f(x) ~ Beta(a,b)", ylim=c(0,4))
i = NULL
for(i in 2:length(as)){
  lines(var, fx(x=var, a=as[i], b=bs[i]), col=i, lwd=4)
}

legend("topright",lwd=c(rep(3,6)),col=c(seq(1,6, by=1)),
       legend=c(paste("(a,b)=", "(", as, ",", bs, ")", sep="")))

######### HW3_3b #################
a= c(1.2, 2,4,10)
b= c(2, 1.5, 2, 5)
fmax <- function(a,b){(a-1)/(a+b-2)}

var2 <- seq(0,1, length=1000)
plot(var2, fx(x=var2, a=a[1], b=b[1]), type="l", lwd=4, 
     ylab="f(x) ~ Beta(a,b)", ylim=c(0,4), xlab="x")
points(fmax(a=a[1], b=b[1]), fx(x=fmax(a=a[1], b=b[1]), a=a[1], b=b[1]), 
       col="orange",pch=18, cex=2)

i = NULL
for(i in 2:length(a)){
  lines(var2, fx(x=var2, a=a[i], b=b[i]), col=i, lwd=4)
  points(fmax(a=a[i], b=b[i]), fx(x=fmax(a=a[i], b=b[i]), a=a[i], b=b[i]),
         col="orange", pch=18, cex=2)
}

legend("topright",lwd=c(rep(3,4)), col=c(seq(1,4, by=1)),
       legend=c(paste("(a,b)=", "(", a, ",", b, ")", sep="")))
legend("topleft", pch = 18, col="orange", cex=1.5 ,
       legend=expression(paste("f((", alpha ,"-1)/(", alpha,"+",beta,"-2))")), bty = "n")


  fmax(a,b)
plot(var2, fx(x=var2, a=a, b=b), type="l", lwd=4, 
     ylab="f(x) ~ Beta(a,b)")

######### HW3_3c #################
#It can be seen that for a>1 and b>1 Beta distributions, 
#the larger the a and b parameters get, distribution becomes less spread (i.e. variation decreses)
# with a higher maximum. It is also known that the maximum of a Beta distribution
# with a>1 and b>1, can be computed via (a-1)/(a+b-2), as the variable that is equal to 
#(a-1)/(a+b-2) gives the maximum density. Therefore, for a dominating density,
# another beta with lower a and b values than the beta distribution of interest can be used.
# Additionally, an optimal c can be calculated by simply computing and divinding the maximum f(x)
#to g(x) by f((a-1)/(a+b-2))/g((a-1)/(a+b-2))
# of f(x) to g(x).
# For example in figure 3

#Figure for 3c
fmax <- function(a,b){(a-1)/(a+b-2)}
var2 <- seq(0,1, length=1000)

max(fx(var2, 5, 3))
fx(fmax(5,3), 5, 3)
max(dbeta(var2, 5,3))

#f(x) a=10, b=6
a1= 2
b1= 2
plot(var2, fx(x=var2, a=a1, b=b1), type="l", lwd=4, 
     ylab="Density", ylim=c(0,1.5), xlab="x")
points(fmax(a1,b1), fx(x=fmax(a1, b1), a1, b1), 
       col="orange", bg="orange",pch=23, cex=2)
#g(x) a=5, b=3
mu=0.5
si=0.35
lines(var2, dnorm(x=var2, mu, si), col=2, lwd=4)
# points(fmax(a2, b2), fx(x=fmax(a2, b2), a2, b2),
#          col="orange", bg="orange",pch=23, cex=2)
c_op <- fx(fmax(a1,b1), a1, b1)/max(dnorm(var2,mu,si))
lines(var2, c_op*dnorm(var2, mu, si), col="dodgerblue", lwd=4)
abline(v=fmax(a1,b1))

legend("bottomright",lwd=c(rep(3,3)), col=c(1,2, "dodgerblue"),
       legend=c(paste("f(X) ~", " Beta(", a1, ",", b1, ")", sep=""), 
                paste("g(X) ~", " N(", mu, ",", si, ")", sep=""),
                paste("c*g(X) ~", " N(", mu, ",", si, ")", sep="")), cex=1)
legend("topleft", pch = 18, col="orange", cex=1 ,
       legend=expression(paste("f((", alpha ,"-1)/(", alpha,"+",beta,"-2))")), bty = "n")


# lines(var2, dt(x=var2,df =  0.5), col=5, lwd=4)
# points(fmax(a2, b2), fx(x=fmax(a2, b2), a2, b2),
#        col="orange", bg="orange",pch=23, cex=2)
# c_op <- fx(fmax(a1,b1), a1, b1)/dbeta(fmax(a2,b2), a2, b2)
# lines(var2, c_op*dbeta(var2, a2, b2), col="dodgerblue", lwd=4)
# legend("topleft", pch = 18, col="orange", cex=1.5 ,
#        legend=expression(paste("f((", alpha ,"-1)/(", alpha,"+",beta,"-2))")), bty = "n")
# 

## 3d ###
# Rejection method for (2,2)
            
# generate samples from N(0.5,0.35)
goverf <- function(x, mu, si, a, b){dnorm(x, mu, si)/fx(x, a, b)}
mu = 0.5
si = 0.35
a= 2
b= 2
#set.seed(6374)
xc = rnorm(1000,mu,si)
uc=runif(1000,0,1)
c_op <- fx(fmax(a,b), a, b)/max(dnorm(var2,mu,si))
tc = c_op*sapply(xc, goverf, mu, si, a, b)
ut = uc*tc
hist(xc[ut <=1], prob=T, main="Rejection via N(0.5,0.35)", xlab="x", ylim=c(0,1.5))
# Now superimpose the standard normal distribution
# on our histogram
x=seq(0,1,length=1000)
lines(x,dbeta(x, 2,2), col="firebrick", lwd=4)
lines(density(xc[ut <=1]), col="dodgerblue", lwd=4)
sum(ut<=1)/1000

# generate samples using uniform
a= 2
b= 2
sims=10000
xb = NULL
bb=NULL
for(i in 1:sims){
  bb <- runif(3,0,1)
  xb[i] <- bb[order(bb)][2]
  }

hist(xb, prob=T, main="Rejection via Uniform distribution theorem", xlab="x")
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

# generate samples using gamma
a= 2
b= 2
sims=10000
rvb = NULL
ga=NULL
gb=NULL
for(i in 1:sims){
  ga <- rgamma (1,a,1)
  gb <- rgamma (1,b,1)
  rvb[i] <- ga/(ga+gb)
}
hist(rvb, prob=T, main="Rejection via Gamma distribution theorem", xlab="x")
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

##plot
par(mfrow = c(2, 2))

hist(xc[ut <=1], prob=T, main="Rejection via N(0.5,0.35)", xlab="x", ylim=c(0,1.5))
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

hist(xb, prob=T, main="Rejection via Uniform distribution \ntheorem", xlab="x")
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

hist(rvb, prob=T, main="Rejection via Gamma distribution \ntheorem", xlab="x")
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

hist(rbeta(seq(0,1,length=1000), 2,2), breaks=20,prob=T, main="Random Variable generation \nvia rbeta() function", xlab="x")
lines(seq(0,1,length=1000),fx(seq(0,1,length=1000), 2,2), col="firebrick", lwd=4)

