library(alr4)
attach(UN11)
install.packages('car')
UN11

#6.4
m1 = lm(lifeExpF ~ log(ppgdp) + group:log(ppgdp))
m2 = lm(lifeExpF ~ group + log(ppgdp) + group:log(ppgdp))
m1
m2
anova(m1,m2)

attach(fuel2001)
fuel2001$Dlic = 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Fuel = 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Income = fuel2001$Income / 1000
attach(fuel2001)
m1 = lm(Fuel ~ Tax + Dlic + Income + log(Miles))
m2 = lm(Fuel ~ 1)
anova(m1,m2)

attach(stopping)
stopping
plot(Distance~Speed)

stopping$Speed2 <- stopping$Speed^2
m1 = lm(Distance~Speed + Speed2,stopping)

library(car)
ncvTest(m1)
ncvTest(m1, ~Speed)
ncvTest(m1, ~Speed + Speed2)
?ncvTest

wt <- 1 / lm(abs(m1$residuals) ~ m1$fitted.values)$fitted.values^2
wls_model <- lm(Distance ~ Speed + Speed2, data = stopping, weights=wt)
new = data.frame(Distance)
predict(m1, new)
predict(m1, new, se.fit=TRUE)$se.fit
predict(wls_model, new)
predict(wls_model, new, se.fit=TRUE)$se.fit
