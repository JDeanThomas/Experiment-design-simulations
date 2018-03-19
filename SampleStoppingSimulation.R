# Optional stopping simulation

N <- 100 
# Number of looks at the data
Looks <- 5 
# Number of simulated studies
nSim <- 50000 
alpha <- 0.05 #set alpha
# Pocock Boundry alpha for 5 looks
#alpha <- 0.0158 

#True effect size (0 for simulating Type 1 errors)
D <- 0 

options(scipen=100, digits=4) 
# Determine at which N's to look
LookN<-ceiling(seq(0, N, N / Looks))
LookN<-LookN[-1] 
# Remove looks at N of 1 or 2 (not possible with t-test)
LookN<-LookN[LookN > 2] 
# If looks are removed, change number of looks
Looks<-length(LookN) 
matp<-matrix(NA, nrow=nSim, ncol=Looks) 
SigSeq<-numeric(Looks) 
OptStop<-numeric(nSim) 
p<-numeric(nSim)

# Loop data generation for each study, then loop to perform a test for each N 
for (i in 1:nSim){
  x<-rnorm(n = N, mean = 0, sd = 1)
  y<-rnorm(n = N, mean = D, sd = 1)
  for (j in 1:Looks){
  matp[i,j]<-t.test(x[1:LookN[j]],y[1:LookN[j]], var.equal=TRUE)$p.value #perform the t-test, store
  }
}

#Save Type 1 error rate for each look
for (i in 1:Looks){
  SigSeq[i] <- sum(matp[,i]<alpha)
}

#Get stopped positions and their P-Values
for (i in 1:nSim){
  OptStop[i] <- min(which(matp[i,]<alpha))
}

# If nothing significant, take last P-Value
OptStop[is.infinite(OptStop)] <- Looks 
for (i in 1:nSim){
  p[i] <- matp[i,OptStop[i]]
}

breaks <- 100
hist(p, breaks=breaks,col="grey")
abline(h=nSim/breaks, col = "red", lty=3)

# Return Type 1 error rates for each look, and the the Type 1 error rate 
# when only reporting the lowest p-value over all looks
cat("Type 1 error rates for look 1 to", Looks,":", SigSeq/nSim)
cat("Type 1 error rate when only the lowest p-value for all looks is reported:", 
    sum(p<alpha)/nSim)

