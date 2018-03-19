# Calculate likelihood ratio fro binomial outcome

n <- 100
x <- 50 
# Null hypothesis (H0) and alternative hypothesis (H1) 
H0 <- .5 
H1 <- 4 / 10 

# Likelihood ratios (H0 over H1, and H1 over H2)
dbinom(x ,n, H0) / dbinom(x, n, H1) 
dbinom(x, n, H1) / dbinom(x, n, H0) 

theta <- seq(0 , 1, len=100) 
like <- dbinom(x, n, theta)

plot(theta, like, type='l', xlab=expression(theta), ylab='Likelihood', lwd=2)
points(H0, dbinom(x, n, H0))
points(H1, dbinom(x, n, H1))
segments(H0, dbinom(x, n, H0), x / n, dbinom(x, n, H0), lty=2, lwd=2)
segments(H1, dbinom(x, n, H1), x / n, dbinom(x, n, H1), lty=2, lwd=2)
segments(x / n, dbinom(x, n, H0), x / n, dbinom(x, n, H1), lwd=2)
title(paste('Likelihood Ratio H0/H1:', round(dbinom(x, n, H0) / dbinom(x, n, H1), digits=2),
           " Likelihood Ratio H1/H0:",round(dbinom(x, n, H1) / dbinom(x, n, H0), digits=2)))