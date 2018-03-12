##### HW6_2 #####

### a ####
claims <- c(rep.int(0, 7840), rep.int(1, 1317), rep.int(2, 239), rep.int(3, 42), 
                   rep.int(4, 14), rep.int(5,4), rep.int(6,4), rep.int(7,1))
b = 3.2
a_mom <- round(b*mean(claims), 3)

### b ###

LF <- NULL ; logLF <- NULL

log_LF <- function(x, a, b = 3.2){
  xi <- unique(x)
  for(i in 1:length(xi)){
    counts <- length(x[x == xi[i]])
    LF[i] <- (b^a/gamma(a))*(gamma(a+ xi[i])/ factorial(xi[i]))*(1/(b+1)^(a+x[i]))
    logLF[i] <- log(LF[i])*counts
  }
  sum(logLF)
}

a_MLE <- optimize(log_LF, c(0.1,1), maximum = T, x=claims)$maximum ## a_MLE

### Plot of log-likelihood function ####
a <- seq(0.000001,100, by=0.1)
log_lfun <- sapply(a, log_LF, x=claims)
par(mar=c(5,6,2,1))
plot( a, log_lfun, type = "l", xlim=c(0,1.5), ylim=c(-3000, -2000),lwd=3, 
     ylab = expression(paste("Log-Likelihood Function of ", alpha)), 
     xlab = expression(alpha), cex.lab =1.5 )

### Bootstrapping for CI ### 
boot_mle = NULL
sim=1000
for(i in 1:sim){
  boot <- sample(claims, length(claims))
  boot_mle[i] <- optimize(log_LF, c(0.1,1), maximum = T, x=boot)$maximum
}

pivotal_alpha = c(2*a_MLE-quantile(boot_mle,.975), 2*a_MLE-quantile(boot_mle,.025))
  

#### 2c ####
prbs=NULL
pi_hat <- function(pr, a, b=3.2){
  for(i in 1:length(pr)){
    prbs[i] <- (b^a/gamma(a))*(gamma(a+ pr[i])/ factorial(pr[i]))*(1/(b+1)^(a+pr[i]))
    }
  sum(prbs)
  }
  
pi_hat_mle <- pi_hat(pr=c(3:7), a=a_MLE)

pi_mle= NULL
for(i in 1:sim){
  boot <- sample(claims, length(claims))
  boot_mle <- optimize(log_LF, c(0.1,1), maximum = T, x=boot)$maximum
  pi_mle[i] <- pi_hat(pr=c(3:7), a=boot_mle)
}

pivotal_pi = c(2*pi_hat_mle-quantile(pi_mle,.975), 2*pi_hat_mle-quantile(pi_mle,.025))

  
  