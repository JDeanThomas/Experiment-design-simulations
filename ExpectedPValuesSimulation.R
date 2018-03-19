# Expected P values simulation

if(!require(pwr)){install.packages('pwr')}
library(pwr)
#Disable scientific notation (1.05e10)
options(scipen=999)

# Number of simulations
nSims <- 100000 

# Mean (IQ), n and SD
# With mean difference of 6, SD of 15, and a n of 26, test has 50% power)
M<-106 
n<-26 
SD<-15 

p <- numeric(nSims) 
bars <- 20

#Run simulation
for(i in 1:nSims){ 
  x <- rnorm(n = n, mean = M, sd = SD) 
  z <- t.test(x, mu=100) 
  p[i] <- z$p.value 
}

# Check power. 
# Sum significant p-values and divide by number of simulations
(sum(p < 0.05)/nSims) #power
#Calculate power formally
# Determines M when power > 0. When power = 0, sets M = 100
power<-pwr.t.test(d=(M - 100) / SD, n=n, sig.level=0.05, type="one.sample", 
                  alternative="two.sided")$power 

# Plot
op <- par(mar=c(5, 7, 4, 4)) 
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
     main=paste("P-value Distribution with", round(power * 100, digits = 1),"% Power"),
     col="grey", xlim=c(0, 1),  ylim=c(0, nSims))
axis(side=1, at=seq(0, 1, 0.1), labels=seq(0, 1, 0.1))
axis(side=2, at=seq(0, nSims, nSims / 4), labels=seq(0, nSims, nSims / 4), las=2)
abline(h=nSims / bars, col="red", lty=3)