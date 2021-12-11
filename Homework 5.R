library(alr4)
library(MASS)
#7.12

#7.12.1
attach(mile)
mile$symb <- ifelse(mile$Gender == 'Female',0,5)
attach(mile)
plot(Year,Time, pch = mile$symb)
legend("bottomleft", title="Gender", legend = c("Female", "Male"), 
       pch = c(0,5))

#7.12.2
male = subset(mile, Gender == 'Male')
female = subset(mile, Gender == 'Female')
row.names(female) <- NULL
m = lm(Time~Year, male)
f = lm(Time~Year, female)
m
f
abline(m)
abline(f)

#7.12.3
inv = lm(Year~Time,female)
inv
plot(Year~Time, female) 
abline(inv)
predict(inv, newdata=data.frame(Time=c(240)), se.fit=T)
grad = c(1,240)
vb <- vcov(inv)
vb
vG <- t(grad) %*% vb %*% grad
sqrt(vG)
install.packages('msm')
library(msm)

#8.2
#8.2.1
attach(stopping)
plot(Distance~Speed)
qqnorm(Distance)
qqline(Distance)
qqnorm(sqrt(Distance))
qqline(sqrt(Distance))
plot(sqrt(Distance)~Speed)

m1 = lm(Distance~Speed + Speed2)
a1 <- powerTransform(m1)
testTransform(a1,c(-1))
testTransform(a1,c(0))
testTransform(a1,c(1))
testTransform(a1,c(2))
Distance
bcPower(Distance, -1)

summary(a3 <- powerTransform(Distance~Speed),stopping)
testTransform(a3, c(1))
testTransform(a3, c(0))
testTransform(a3, c(-1))
testTransform(a3, c(1/2))

fm1 <- lm(Distance~Speed, stopping)
fm2 <- lm(Distance~Speed+Speed2,stopping)
logLik(fm2)
logLik(fm1)

