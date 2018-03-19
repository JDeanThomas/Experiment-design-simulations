# CI mean difference simulation

# Group means, SDs and Ns
m1 <- 3 
m2 <- 5 
sd1 <- 4.5 
sd2 <- 3.0 
n1 <- 50 
n2 <- 50 

# Gernate samples
x<-rnorm(n = n1, mean = m1, sd = sd1) 
y<-rnorm(n = n2, mean = m2, sd = sd2) 
# Calculate error terms
error1 <- qnorm(0.975) * sd(x) / sqrt(n1) 
error2 <- qnorm(0.975) * sd(y) / sqrt(n2) 
# Calculate confidence intervals
CI_l_1 <- mean(x)-error1 
CI_u_1 <- mean(x)+error1
CI_l_2 <- mean(y)-error2
CI_u_2 <- mean(y)+error2

# Calculate pooled standard error
se <- sqrt(sd(x)*sd(x)/n1+sd(y)*sd(y)/n2)
# Error mean difference
error <- qt(0.975, df=n1 + n2 - 2) * se 
# Mean difference
mdif<-mean(y)-mean(x)
# CI differences
CI_l_d <- mdif-error
CI_u_d <- mdif+error 

d = data.frame(labels=c("X","Y","Difference"), 
               mean=c(mean(x),mean(y),mdif),
               lower=c(CI_l_1,CI_l_2,CI_l_d),
               upper = c(CI_u_1,CI_u_2,CI_u_d))

plot(NA, xlim=c(.5, 3.5), ylim=c(0, max(d$upper[1:2] + 1)), bty="l", xaxt="n",
                                                        xlab="", ylab="Mean")
points(d$mean[1:2], pch=19)
segments(1, d$mean[1],5, d$mean[1], lty=2)
segments(2, d$mean[2],5, d$mean[2], lty=2)
axis(1, 1:3, d$labels)
segments(1:2, d$lower[1:2], 1:2, d$upper[1:2])
axis(4, seq((d$mean[1]-3), (d$mean[1]+5), by=1), seq(-3, 5, by=1), las=1)
points(3, d$mean[1] + d$mean[3], pch=19, cex=1.5)
segments(3, d$mean[1] + d$lower[3], 3, d$mean[1] + d$upper[3], lwd=2)
mtext("Difference", side=4, at=d$mean[1], line=3)
segments(1:1, d$upper[1:1], 1:2, d$upper[1:1], lty=3)
segments(1:1, d$lower[1:2], 1:2, d$lower[1:2], lty=3)
text(3, 1, paste("P-value", round(t.test(x, y, var.equal=TRUE)$p.value, digits=3)), cex = .8)

