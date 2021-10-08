install.packages("alr4")
library(alr4)
#1.2
wblake
attach(wblake)
tapply(Length, Age, mean)
tapply(Length, Age, var)
tapply(Scale, Age, mean)
tapply(Scale, Age, var)
avgs <- aggregate(Length~Age, data=wblake,mean)
plot(tapply(avgs$Length, avgs$Age, mean), xlab='Age',ylab='Average Length')
abline(lm(avgs$Length~avgs$Age))

plot(tapply(Length,Age, sd), ylab='Standard Deviation of length', xlab='Age')
summary(tapply(Length,Age, sd))

#1.6
attach(Rateprof)
my_data <- Rateprof[, c(8,9,10, 11, 12)]
cor(my_data)

#2.2
attach(UBSprices)
UBSprices$difference <- (rice2009 - rice2003)
sorted <- UBSprices[order(UBSprices$difference, decreasing = TRUE),]
sorted

#2.4

#2.4.1
plot(bigmac2003, bigmac2009)
abline(0,1)
abline(lm(bigmac2003~bigmac2009), col='red', lty=2)
legend(115, 50, legend = c("Y=X","2009 vs. 2003"), col = c('black','red'), 
       lty=1:2, cex = .8)
sorted
row.names(UBSprices)
text(bigmac2003, bigmac2009, labels = row.names(UBSprices), cex=.7, pos = 3)

#2.4.3
plot(log(bigmac2003), log(bigmac2009))
abline(0,1)
abline(lm(log(bigmac2003)~log(bigmac2009)), col='red', lty=2)
text(log(bigmac2003), log(bigmac2009), labels = row.names(UBSprices), cex=.7,
     pos=3)

E(Ybar^2) - (E(Ybar))^2 
