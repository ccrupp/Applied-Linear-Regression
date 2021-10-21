library(alr4)

#4.2
attach(Transact)
df = Transact
df['a'] = (t1+t2)/2
df['d'] = (t1-t2)
m1 = lm(df$time~t1+t2,df)
m1
m2 = lm(df$time~df$a+df$d, df)
m2
m3 = lm(df$time~df$t2+df$d, df)
m3
m4 = lm(df$time~df$t1+df$t2+df$a+df$d, df)
m4

?UN11

#5.4.1
attach(MinnLand)
m1 = lm(log(acrePrice) ~ year + region + year:region)
m2 = lm(log(acrePrice) ~ year + region)
m3 = lm(log(acrePrice) ~ region + year + region:year)
summary(m3)
m3 = lm(log(acrePrice) ~ year + region + region:year)
summary(m3)
summary(m1)
summary(m2)
?MinnLand
boxplot(log(acrePrice)~year, MinnLand)

#5.4.2
MinnLand$factoryear = as.factor(MinnLand$year)
attach(MinnLand)
m1 = lm(log(acrePrice)~factoryear, MinnLand)
summary(m1)

#5.4.3
m1 = lm(log(acrePrice) ~ fyear-1, data=MinnLand)
coefficients(m1)
with(MinnLand, tapply(log(acrePrice), MinnLand$year, mean))
summary(m1)
with(MinnLand, tapply(log(acrePrice), MinnLand$year, function(x) sd(x) / sqrt(length(x))))

#5.10.1
a = lm(log(acrePrice) ~ factoryear + region)
b = lm(log(acrePrice) ~ factoryear + region + factoryear:region)
summary(a)
summary(b)

#5.10.2
plot(allEffects(b))
