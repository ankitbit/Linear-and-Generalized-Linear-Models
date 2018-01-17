#setwd('E:/GLM/Problemas/20171024')
dd = read.csv2("./data/GMD.csv")

install.packages('multcompView')
library(car)
library(lsmeans)
library(tables)
library("RcmdrMisc")
library('multcompView')

dd$DOSI = as.factor(dd$DOSI)
is.factor(dd$DOSI)
mod = lm(Y ~ DOSI, data = dd)
summary(mod)

#Anove tipo 1
anova(mod)

#Anove tipo 3
Anova(mod, ty = '3')

#lsmeans
lsm = lsmeans(mod, ~DOSI)
lsm

#Compare all pairs of combinations 
pairs(lsm)


#Make group Within a group I can not distinguish them
cld(lsm, alpha = 0.01)

#Graphic view of the different levels. Intuitively you see those that are the same, etc.
plot(lsm, level = 0.99)

#lsm already gives the confidence interval individually.
#incld the alpha is what you ask to differentiate the groups. At 0.01 you know what
# the 1 is different from the 2 with a confidence of 0.01
confint(lsm, level = 0.99)

# what dose do we choose? We are interested in fattening the pig we would choose on 15, 20 or 30.
# But we can not distinguish between them

#Diagnostics
# 5 groups have to go outplot(predict(mod), resid(mod) )
abline(h = 0, lty = 2)

plot(rstudent(mod))

#leverage: no tiene sentido
hatvalues(mod)
dffits(mod)

# Equality variance test
#Sale not significant.
# h0: the variances are the same
# h1: There is a group with different variances
leveneTest(mod)
leveneTest(Y ~ DOSI, dd)
bartlett.test(Y ~ DOSI, dd)


#Normality has to do with normality. There are several tests
plot(mod, ask = FALSE)

shapiro.test(mod$residuals)


shapiro.test(rnorm(100, mean = 5, sd = 3))


shapiro.test(runif(100, min = 2, max = 4))
#Idenpendecia: if by the experiment
#Normality: We can not affirm it
#Variances homo: We can not affirm it