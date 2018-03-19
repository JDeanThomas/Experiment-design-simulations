# Binomial Bayes Factor

# Null hypothesis
H0 <- .5 
# Trials and successes
n <- 50 
x <- 20 

# alpha and beta for the Beta distribution for the prior
aprior <- 2 
bprior <- 2 
# alpha and beta for the Beta distribution for the likelihood
alikelihood <- x + 1 
blikelihood <- n - x + 1 
# alpha and betafor the Beta distribution for the posterior
aposterior <- aprior + alikelihood - 1 
bposterior <- bprior + blikelihood - 1 

# Range from 0 to 1
theta <- seq(0, 1, 0.001) 

prior <- dbeta(theta, aprior, bprior)
likelihood <- dbeta(theta, alikelihood, blikelihood)
posterior <- dbeta(theta, aposterior, bposterior)
plot(theta, posterior, ylim=c(0, 15), type="l", lwd=3, xlab=bquote(theta), ylab="Density", las=1)
lines(theta, prior, col="grey", lwd=3)
lines(theta, likelihood, lty=2, lwd=3, col="dodgerblue")
BF10 <- dbeta(H0, aposterior, bposterior)/dbeta(H0, aprior, bprior)
points(H0,dbeta(H0, aposterior, bposterior), pch=19)
points(H0,dbeta(H0, aprior, bprior), pch=19, col="grey")
segments(H0, dbeta(H0, aposterior, bposterior), H0, dbeta(H0, aprior, bprior), lty=2)
title(paste('Bayes Factor:', round(BF10, digits=2)))