######### Part 1 ##############
seed(2580)
x <- runif(1000,0,1)
y <- x*pi-(pi/4)

par(mfrow=c(1,2), bg="gray95")
hist(x, breaks=50,main="Histogram of X", xlab="Random Variables", ylim=c(0,30))
hist(y, breaks = 50,main="Histogram of Y", xlab="Random Variables",ylim=c(0,30))

x <- runif(1000,0,1)
y <- x*pi-(pi/4) 
y_counts<-hist(y, breaks=50,main="Histogram of X")$counts
chisq.test(y_counts)

qnorm(0.95, mean=1100, sd=75)

seed(2459)
z <- runif(1000,0,1)
xz <- x+z 

par(mfrow=c(1,2))
hist(xz,breaks=20, main="Histogram of X+Z", xlab="Random Variables", col="firebrick")
abline(density(xz))
truehist(xz, xlab="Random Variables", main=" True Histogram of X+Z")

######### Part 2 ##############
install.packages('xtable') 
library(xtable) #package to extract table in LaTeX format
setwd("./Google Drive/Statistical Computing/HW1/")

roster <- read.table("./classlist.txt", header = T, stringsAsFactors = F, sep = "\t")
chrs <- roster$Program.and.Plan
roster$Program.and.Plan <- gsub('.{3}$', '', chrs)
roster$Grade <- sample(60:100, 35, replace=T)
intervals <- cut_interval(roster$Grade, length = 5)
levels(intervals) <- c("C", "C", "C", "B-", "B", "B+", "A-", "A")
roster$Letter_Grade <- intervals
roster.table <- xtable(roster)
print(roster.table) #extracts table in the LaTeX format to the R console
