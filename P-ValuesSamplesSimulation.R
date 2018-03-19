# Simulation of variation of P-Values as function of sample size

# Number of observations (per condition) to collect after initial 10
n <- 500 
# True effect size 
D <- 0.2
# Fix true SD at 1
SD <- 1 

x <- numeric(n)
y <- numeric(n)
p <- numeric(n) 

n <- n + 10 

for(i in 10:n){ 
  x[i] < -rnorm(n=1, mean=0, sd=SD)
  y[i] <- rnorm(n=1, mean=D, sd=SD)
  z    <- t.test(x[1:i], y[1:i], var.equal=TRUE) 
  p[i] <- z$p.value 
}

#Remove first 10 P-Values
p <- p[10:n] 

plot(0, col="red", lty=1, lwd=3, ylim=c(0, 1), xlim=c(10, n), type="l", 
     xlab='sample size', ylab='p-value', cex.lab=1, cex.axis=1, xaxt = "n")
lines(p, lwd=2)
abline(h=0.05, col="darkgrey", lty=2, lwd=2) #draw ine at p = 0.05
axis(1, at=seq(0, n - 10, by=(n - 10) / 4), labels = seq(10, n, by=(n - 10) / 4))


# Return lowest P-Value
min(p)

# Print sample size at which the p-value was smallest
cat("The lowest p-value was observed at sample size", which.min(p)+10) 
# Print sample size at which the p-value dropped below 0.05 for the first
cat("The p-value dropped below 0.05 for the first time as sample size", 
    which(p<0.05)[1]+10) 

