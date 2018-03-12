x= seq(-5,10,by= 0.1)
y= 1/(5-x)
plot(x,y, type="l", lwd=3)
abline(h=0, lwd=3, col="gray")
abline(v=0, lwd=3, col="gray")
abline(v=5, lwd=3, lty=3, col="gray")

### c ####
nsim <- 10000
set.seed(234829)
xi <- runif(10, 1, 5)
t_MLE = NULL ; t_MOM = NULL
for(i in 1:nsim){
  boot <- sample(xi, 10, replace = T)
  t_MLE[i] <- (5+min(boot))/2
  t_MOM[i] <- mean(boot)
}

T_mle <- (5+min(xi))/2
T_mom <- mean(xi)

CI_mle <- c(quantile(t_MLE,.025), quantile(t_MLE, .975))
CI_mom <- c(quantile(t_MOM,.025),quantile(t_MOM,.975))

