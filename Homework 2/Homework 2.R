library(alr4)

#2.14

#2.14.1
attach(Heights)
construct.sample = sample(1:dim(Heights)[1], floor(dim(Heights))[1]*(2/3))

#2.14.2
m1 = lm(mheight~dheight, data=Heights, subset=construct.sample)
result <- Heights[-construct.sample,]
result['Prediction'] = predict(m1, newdata = Heights[-construct.sample,])
result['Residual'] = (result['Prediction'] - mheight)^2
n = nrow(result)
sqres <- sum(result['Residual']) / n
sqres
sqrt(sqres)

#2.14.3
#Create the linear model
result['sepred'] = sd(dheight) * (1 + 1/n + 
  (result['dheight'] - mean(dheight))^2)^(1/2)
sum(result['sepred'] / n)
sqrt(sum(result['sepred']) / n)

#2.16
#2.16.1
attach(UN11)
line = lm(fertility~ppgdp, data = UN11)
plot(ppgdp,fertility)
abline(line)

x = log(ppgdp)
y = log(fertility)
line = lm(y~x, data = UN11)
#2.16.2
plot(x,y)
abline(line)

#2.16.3
res <- summary(line)
pt(coef(res)[, 3], line$df, lower = TRUE)
tstat = coef(res)[2,1] / coef(res)[2,2]
tstat
pt(tstat, 197, lower.tail=TRUE)
summary(line)
# Reject the null hypothesis, the slope is <= 0.

#2.16.4
summary(line)
#R^2 shows how well the predictions from the regression line approximate real
#data points. X % of the variance in the dependent variable is explained by 
#the independent variable.

#2.16.5
UN11
plot(log(ppgdp), log(fertility))
mod = lm(log(fertility)~log(ppgdp), data = UN11)
dat = data.frame(ppgdp = c(1000))
#1.234567 is the predicted value when ppgdp is 1000
predict(mod, dat, interval='predict')
a = predict(mod, dat, interval='predict')[2]
b = predict(mod, dat, interval='predict')[3]
plot(ppgdp, fertility)
exp(a)
exp(b)

#3.6

#3.6.1
attach(water)
mod = lm(BSAAM~OPBPC+OPRC+OPSLAKE)
summary(mod)
pairs(~BSAAM+OPBPC+OPRC+OPSLAKE,data=water, 
      main="Simple Scatterplot Matrix")
cor(water[, c(5,6,7,8)])
#The correlation matrices should all be positively correlated with BSAAM and 
#OPSLAKE being the most positively correlated, OPRC being the second, and 
#OPBPC being the third.

#3.6.2
summary(mod)
#These T-values are the test statistics calculated from our estimate/standard
#error and when used in a t-test, it tells us how likely our estimate is given
#our calculated t value under the null hypothesis (B0 = 0  in this case).
#When the p-value is calculated (Probability to get a test statistic that is
#not our calculated one) we can use this to determine how likely our estimate
#of B* is. In this case, our estimate of b corresponding to OPBPC is unlikely
#but the other two are, this signals that the relationship between OPBPC
# and BSAAM is likely due to chance and probably should not be included in
#the model




