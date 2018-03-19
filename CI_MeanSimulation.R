require(ggplot2)

n = 20 #set sample size
nSims <- 100000 #set number of simulations
#set.seed(1000)

x<-rnorm(n = n, mean = 100, sd = 15) #create sample from normal distribution

#95% Confidence Interval
CIU<-mean(x)+qt(0.975, df = n-1)*sd(x)*sqrt(1/n)
CIL<-mean(x)-qt(0.975, df = n-1)*sd(x)*sqrt(1/n)

#95% Prediction Interval
PIU<-mean(x)+qt(0.975, df = n-1)*sd(x)*sqrt(1+1/n)
PIL<-mean(x)-qt(0.975, df = n-1)*sd(x)*sqrt(1+1/n)

#plot data
ggplot(as.data.frame(x), aes(x))  + 
  geom_rect(aes(xmin=PIL, xmax=PIU, ymin=0, ymax=Inf), fill="gold") + 
  geom_rect(aes(xmin=CIL, xmax=CIU, ymin=0, ymax=Inf), fill="#E69F00") + 
  geom_histogram(colour="black", fill="grey", aes(y=..density..), binwidth=2) +
  xlab("IQ") + ylab("number of people")  + ggtitle("Data") + 
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), 
  panel.grid.minor.x = element_blank()) + 
  geom_vline(xintercept=mean(x), colour="black", linetype="dashed", size=1) + 
  coord_cartesian(xlim=c(50,150)) + scale_x_continuous(breaks=seq(50, 150, 10)) +
  annotate("text", x = mean(x), y = 0.02, label = paste("Mean = ", round(mean(x)),
                                   "\n","SD = ", round(sd(x)), sep=""), size=6.5)


#Simulate Confidence Intervals
CIUSim<-numeric(nSims)
CILSim<-numeric(nSims)
meanSim<-numeric(nSims)

for(i in 1:nSims){ 
  x <- rnorm(n = n, mean = 100, sd = 15) 
  CIUSim[i] <- mean(x) + qt(0.975, df=n - 1) * sd(x) * sqrt(1 / n)
  CILSim[i] <- mean(x) - qt(0.975, df=n - 1) * sd(x) * sqrt(1 / n)
  meanSim[i] <- mean(x) 
}

# Only save simulations where the true value was inside the 95% CI
CIUSim <- CIUSim[CIUSim < 100]
CILSim <- CILSim[CILSim > 100]

cat((100 * (1 - (length(CIUSim) / nSims + length(CILSim) / nSims))),
       "% of the 95% confidence intervals contained the true mean")

#Calculate number of times observed mean fell within 95% CI of the original study
meanSim<-meanSim[meanSim>CIL&meanSim<CIU]
cat("The capture percentage for the plotted study, or the % of values within 
the observed confidence interval from", CIL, "to", CIU,"is:",100*length(meanSim)/nSims,"%")

