#################  HW3_2 #########################

### HW3_2_a ###

laplace <- function(x, theta=1){(theta/2)*exp(-(theta)*abs(x))}
cauchy <- function(x, mu=3){mu/(pi*(mu^2+x^2))}
myx <- c(seq(-5, -1, length=1000), seq(-1, 1, length= 10000), seq(1, 5, length= 1000))
phi <- laplace(myx)/cauchy(myx)
# plot(myx, phi, type="l", lwd=4, ylab=" ", xlab= "Variables", col="forestgreen")
plot(myx, laplace(myx), lwd=4, ylab= "Density", type = "l", xlab= "x")
lines(myx, laplace(myx), lwd=4, col="black")
lines(myx, cauchy(myx), lwd=4, col="blue")
lines(myx, max(phi)*cauchy(myx), lwd=4, col="red")
#lines(myx, phi, lwd=4, col="forestgreen")
legend("topright",lwd=c(4,4,4),col=c("black","blue", "red"),
       legend=c("f(x)","g(x)", "ag(x)"))

# legend("topright",lwd=c(4,4,4),col=c("black","blue", "red", "forestgreen"),
#        legend=c("f(x)","g(x)", "ag(x)", "f(x)/g(x)"))

a =round(max(phi), 3) #4.712

### HW3_2_b ###
# pdf("~/myplot.pdf", width=5.05, height=3.8)
# mar.default <- c(5,4,4,2) + 0.1
# par(mar = mar.default + c(0, 4, 0, 0)) 

# We can use inversion method, where the cdf can be found by integrating pdf and
# by finding the inverse function of the CDF.

g_cdf <- function(x){atan(x/3)/pi}
inv_g_cdf <- function(x){3*tan(pi*x)}
set.seed(84)
uni <- runif(1000, 0,1)
xc <- inv_g_cdf(uni)
hist(xc, probability = T, xlim= c(-5,5), breaks=seq(-1000, 600, by=1),
     main= "Histogram of generated Cauchy random variables", ylim= c(0, .12))
df <- data.frame(xc, cauchy(xc))
df2<- df[order(df),]
lines(df2, lwd=4)

### HW3_2_c ###
goverf <- function(x){cauchy(x)/laplace(x)}
set.seed(230)
uc=runif(1000,0,1)
tc = a*sapply(xc, goverf)
ut = uc*tc
hist(xc[ut <=1], prob=T, xlim=c(-4,4), ylim = c(0,0.5), breaks=seq(-4,6,by=0.5), 
     main="Histogram of generated random variables \nfrom Laplace distribution",
     xlab="xc")
# Now superimpose the standard normal distribution
# on our histogram
x=seq(-4,4,length=1000)
lines(x,laplace(x), lwd=4)

sum(ut<=1)/1000
