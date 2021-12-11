library(alr4)
Forbes
#9.10
n = 54
pprime = 5
varhat = 4

ehat = 1
hii = .9
ri = ehat/(varhat*sqrt(1-hii))
ri
ti = ri*sqrt(((n-pprime-1)/(n-pprime-ri^2)))
ti
di = (1/pprime) * ri^2 * (hii/(1-hii))

di

ehat = 1.732
hii = .75
ri = ehat/(varhat*sqrt(1-hii))
ri
ti = ri*sqrt(((n-pprime-1)/(n-pprime-ri^2)))
ti
di = (1/pprime) * ri^2 * (hii/(1-hii))
di

ehat = 9
hii = .25
ri = ehat/(varhat*sqrt(1-hii))
ri
ti = ri*sqrt(((n-pprime-1)/(n-pprime-ri^2)))
ti
di = (1/pprime) * ri^2 * (hii/(1-hii))
di

ehat = 10.195
hii = .185
ri = ehat/(varhat*sqrt(1-hii))
ri
ti = ri*sqrt(((n-pprime-1)/(n-pprime-ri^2)))
ti
di = (1/pprime) * ri^2 * (hii/(1-hii))
di

t.test(statistics = .784441, parameter = n - p - 1)

#10.3
attach(mantel)
mantel

min.model = lm(Y~1, data=mantel)
full <- formula(lm(Y~X1+X2+X3,mantel))
fwd.model = step(min.model, direction='forward', scope=full)
fwd.model$coefficients
fwd.model$anova
mantel

options(warn=-1)
max.model = lm(Y~X1+X2+X3, data=mantel)
min <- formula(lm(Y~1,mantel))
bckwd.model = step(max.model, direction='backward', scope=full)
bckwd.model$coefficients
bckwd.model$anova
mantel

m1 = lm(Y~X1 + X2 + X3, data=mantel)
m2 = lm(Y~X1 + X2, data=mantel)
m3 = lm(Y~X1, data=mantel)
m4 = lm(Y~1, data=mantel)
AIC(m1)
BIC(m1)
AIC(m2)
BIC(m2)
AIC(m3)
BIC(m3)
AIC(m4)
BIC(m4)
