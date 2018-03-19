# Bayesian Binomial Posterior Mean

if(!require(binom)){install.packages('binom')}
library(binom)

# Trials and successes
n <- 20 
x <- 12 

# alpha and beta for the Beta distribution for the prior
aprior <- 10 
bprior <- 10 

# Max y-axis
ymax <- 10 

# alpha and beta for the Beta distribution for the likelihood
alikelihood <- x + 1 
blikelihood <- n - x + 1 
# alpha and betafor the Beta distribution for the posterior
aposterior <- aprior + alikelihood - 1 
bposterior <- bprior + blikelihood - 1 

# Draw distributions
theta<-seq(0,1,0.001) 
prior <- dbeta(theta, aprior, bprior) 
likelihood <- dbeta(theta, alikelihood, blikelihood) 
posterior <- dbeta(theta, aposterior, bposterior) 

plot(theta, posterior, ylim=c(0, ymax), type = "l", lwd = 3, xlab = bquote(theta), 
                                                       ylab = "Density", las = 1) 
lines(theta, prior, col="grey", lwd=3) 
lines(theta, likelihood, lty=2, lwd=3, col="dodgerblue")
# Calculate Bayesian Credible Interva
LL <- qbeta(.025, aposterior, bposterior) 
UL <- qbeta(.975, aposterior, bposterior)
# Mean line
abline(v=aposterior / (aposterior + bposterior))
# CI lines
abline(v = LL, col="grey",lty=3) 
abline(v = UL, col="grey",lty=3) 
polygon(c(theta[theta < LL], rev(theta[theta < LL])), c(posterior[theta < LL], 
                        rep(0,sum(theta < LL))), col="lightgrey", border=NA)
polygon(c(theta[theta > UL], rev(theta[theta > UL])), c(posterior[theta > UL], 
                        rep(0, sum(theta > UL))), col="lightgrey", border=NA)
title(paste('Mean posterior:', round((aposterior / (aposterior + bposterior)), 
                  digits=5), ", 95% Credible Interval:", round(LL , digits=2), 
                  ";", round(UL , digits=2)))


binom.bayes(x, n, type="central", prior.shape1=aprior, prior.shape2=bprior)
binom.bayes(x, n, type="highest", prior.shape1=aprior, prior.shape2=bprior)

