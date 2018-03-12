#################     HW3_1       ########################
##########################################################

#### HW3_1_a ####
xseq <- seq(0, 3, length=10000)[-1]
fx <- function(x){2/(pi*(1+x)*sqrt(x^2+2*x))}
cg1 <- function(x){2/(pi*sqrt(2*x))}
cg2 <- function(x){2/(pi*x^2)}
plot(xseq, y=fx(xseq), ylim=c(0,3), type="l", lwd=4, ylab= " ", 
     xlab = expression(italic(x)), cex.lab=2)
lines(xseq, y= cg1(xseq), col="forestgreen", lwd=4)
lines(xseq, y= cg2(xseq), col="blue", lwd=4)
a=2^(1/3)
points(a, y= cg1(a),col="red", pch=16)
#points(a, y= 2/(pi*a^2),col="orange", pch=16)

text( 1.5, y= 2/(pi*a^2), cex=1, pos= 3.4, labels = expression(paste("x = ", 2^(1/3))), 
                                                                 font=4, col="red")

text( 0.4, -0.15, cex=0.8, pos= 3.4, 
      labels = expression(paste("f(x)", "=", frac(2, pi*(1+x)*sqrt(x^2 + 2*x)))),  font=4)

text( 0.24, 2.6, cex=0.7, pos= 3.4, 
      labels = expression(paste(c[theta],g[theta], "=", 
                                frac(2, pi*sqrt(2*x)))),  font=4, col="forestgreen")

text( 0.755, 2, cex=0.8, pos= 3.4, 
      labels = expression(paste(c[theta],g[theta], "=", 
                                frac(2, pi*x^2))),  font=4, col="blue")

### HW3_1_b ###
theta = NULL
c_theta <- function(theta){(2/pi)*(sqrt(2*theta)+(1/theta))}
tseq <- seq(0, 10, length=10000)[-1]
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
plot(tseq, c_theta(tseq), xlim=c(0, 10), ylim=c(0,10), cex.lab=2, 
     xlab = expression(theta), ylab = expression(c[theta]), type = "l", lwd=6,
     main=expression(paste(c[theta], " ", "as a function of ", theta)), cex.main=2)
points(a, c_theta(a), col="red", pch=16, cex=1)
text(a-0.5, c_theta(a)+1, cex=1.5, pos = 4, col='red',
     labels=expression(paste(theta, "=", 2^(1/3)))) 
min(c_theta(tseq))
a=2^(1/3) #1.515856
c_theta(a) #1.515856

### HW3_1_c ###
  x <- NULL
cgx <- function(x, theta=a){
  if (x <= theta) {
    aa <- cg1(x)
    
  } else {
    aa <- cg2(x)
  }
  return(aa)
}

goverf <- function(x, theta=a){
  if (x <= theta) {
    aa <- cg1(x)/fx(x)
    
  } else {
    aa <- cg2(x)/fx(x)
  }
  return(aa)
}

nsims=500
# guesses=list()
iter <- NULL
solutions <- NULL
set.seed(2211)
random <- runif(1000)


for(n in 1:nsims){
  
  x0 <- 0.5
  x1 <- NULL
  i <- 1
  N=2000
  p <- NULL
  
  u <- random[n]
  
  while(i <= N){
    x1 <- x0 - (c_theta(a) - u)/cgx(x0)
    p[i] =x1
    i = i+1
    if(abs(x1 - x0) < 0.05) {break}
    x0 = x1
  }
  
  solutions[n] <- x1
  iter[n] <- i
  # guesses[[n]] <- p 
}


set.seed(3456)
xu <- runif(500, 0, 8)
xg <- sapply(xu, cgx)/c_theta(a)
t <- sapply(xg, goverf)
set.seed(1098)
u <- runif(500,0,1)
ut = u*t
sum(ut<=1)/500

mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 0, 0, 2)) 

hist(xg[ut <=1], prob=T,  ylim=c(0,4), xlim=c(0,3), breaks=seq(0,9, by=0.1), 
     col="gray80", xlab="Accepted Observations", main="Histogram of Accepted Observations")

lines(xseq, y=fx(xseq), lwd=4, col="firebrick")
text( 2.5, 0.16, cex=1, pos= 3.4, 
      labels = expression(paste("f(x)", "=", frac(2, pi*(1+x)*sqrt(x^2 + 2*x)))),  
      font=4, col="firebrick")

#########################################################################################
#########################################################################################


