##########################
##### 2. Newton Raphson ####

# unnecessary visualizations
# set.seed(100)
# bdist <- rbeta(1000, shape1 = 3, shape2 = 5)
# Fx_bdist <- pbeta(bdist, 3, 5)
# plot(bdist,Fx_bdist, pch=16, col="black" , xlab="Random Variables", ylab="F(X)")
# points(bdist[75], Fx_bdist[75], pch=16, col="red", cex=2)
# text(bdist[75], Fx_bdist[75], cex=2, pos= 2, labels = "u", font=4)
# ux<- bdist[75]
# uy<- Fx_bdist[75]
# phi_Fx <- Fx_bdist - uy
# plot(bdist, phi_Fx, xlab="Random Variables", ylab= "phi(x)")
# abline(h=0)
# abline(v=0)
# Fx<-as.data.frame(cbind(bdist, Fx_bdist))
# points(bdist[75], phi_Fx[75], pch=16, col="red", cex=2)
# text(bdist[75], phi_Fx[75], cex=2, pos= 2, labels = "u", font=4)
# 
# phi_x <- cbind(bdist, phi_Fx)
# x1 <- 0.5
# newton <- x1 - ((pbeta(x1, 3, 5)-uy)/dbeta(x1, 3, 5))
# x2 <- 0.3
# abs(x2-x1)
# plot(density(bdist))
# plot(bdist , dbeta(bdist, 3, 5))

nsims=100
# guesses=list()
iter <- NULL
solutions <- NULL
set.seed(2211)
random <- runif(1000)

for(n in 1:nsims){
  
  x0 <- 0.5
  x1 <- NULL
  i <- 1
  N=200
  p <- NULL
  
  u <- random[n]
  
  while(i <= N){
    x1 <- x0 - (pbeta(x0, 3, 5) - u)/dbeta(x0, 3, 5)
    p[i] =x1
    i = i+1
    if(abs(x1 - x0) < 0.05) {break}
    x0 = x1
  }
  
  solutions[n] <- x1
  iter[n] <- i
  # guesses[[n]] <- p 
}


mean(iter) # Average Number of Iterations

library(MASS)
par(mfrow=c(1,2))
hist(solutions, col="gray70", xlim = c(-0.13, 1), 
     xlab="Accepted Observations", main="Histogram of Accepted Observations")
truehist(solutions, col="gray70", ymax = 2.5, xlim = c(-0.13, 1), 
         xlab="Accepted Observations", main="True Histogram of Accepted Observations")
lines(density(solutions)$x,density(solutions)$y, col="red", lwd=5)
beta.pdf <- data.frame(solutions, dbeta(solutions, 3, 5)) 
colnames(beta.pdf) <- c("x", "y")
beta.pdf<-beta.pdf[order(beta.pdf$x),]
lines(beta.pdf$x, beta.pdf$y, col="blue", lwd=5)
#legend("topright", c("Emprical Density", "Theoretical Density"),lwd=c(5, 5), col=c("red", "blue"))

##################################################
#### Chi-Squared test and Kolmogorov-Smirnov #####

## 3.a) ##
sims=500
size <- c(10, 25, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
# empty data frame
results <- data.frame(matrix(c(1:2*length(size)), nrow = length(size), ncol = 2))
colnames(results) <- c("Kolmogorov-Smirnov Test", "Chi-Squared Test")
rownames(results) <- paste("n", size, sep="=")

for(n in 1:length(size)){
  p_KS <- NULL
  p_CS <- NULL
  for(i in 1:sims){
    x <- round(runif(size[n]), 3)
    p_KS[i] <- ks.test(x, "punif", 0, 1)$p.value
    xcount <- hist(x, breaks=10, plot=F)$counts
    p_CS[i] <- chisq.test(xcount)$p.value
  }
  KS <- sum((p_KS <= 0.05)*1)
  CS <- sum((p_CS <= 0.05)*1)
  results[n, 1:2] <- cbind(KS, CS)
}  

library(xtable)
results <- results/sims
xtable(results)

#3.b)

sims=500
size <- c(10, 25, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
results <- data.frame(matrix(c(1:2*length(size)), nrow = length(size), ncol = 2))
colnames(results) <- c("Kolmogorov-Smirnov Test", "Chi-Squared Test")
rownames(results) <- paste("n", size, sep="=")

for(n in 1:length(size)){
  p_KS <- NULL
  p_CS <- NULL
  for(i in 1:sims){
    x <- round(rbeta(size[n], 4, 6), 3)
    p_KS[i] <- ks.test(x, "pbeta", 0, 1)$p.value
    xcount <- hist(x, breaks=10, plot=F)$counts
    p_CS[i] <- chisq.test(xcount)$p.value
  }
  KS <- sum((p_KS <= 0.05)*1)
  CS <- sum((p_CS <= 0.05)*1)
  results[n, 1:2] <- cbind(KS, CS)
}  

results <- results/sims
library(xtable)
xtable(results)


#4
set.seed(333)
sims=500
size <- c(10, 25, 50, 100, 200, 500, 1000, 2000, 5000)
bins <- c(seq(5, 50, by=5), seq(100, 500, by=100))
results <- data.frame(matrix(c(length(bins)*length(size)), 
                             nrow = length(bins), ncol = length(size)))
colnames(results) <- as.character(size)
rownames(results) <- as.character(bins)

for(n in 1:length(size)){
  
  for(i in 1:length(bins)){
    
    p_CS <- NULL
    
    for(j in 1:sims){
      x <- round(runif(size[n]), 3)
      xcount <- hist(x, breaks=bins[i], plot=F)$counts
      p_CS[j] <- chisq.test(xcount)$p.value
    }
    CS <- sum((p_CS <= 0.05)*1)
    results[i,n] <- CS
  }
}  
results <- results/sims
par(mfrow=c(3,3))
for(c in 1:length(size)){
  barplot(results[,c], names.arg = as.character(bins), main = paste("Size n", size[c], sep="="), 
          xlab="# of Bins", ylab="Frequency of p-values < 0.05", ylim = c(0,0.1))
  # abline(lm(results[,c]~bins),col="red",lwd=4)
}
results <- results/sims
library(xtable)
xtable(results)

