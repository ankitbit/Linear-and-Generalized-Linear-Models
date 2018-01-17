# 
# We want to compare the evaluations (v) of two pedagogical methods (m) according to the coefficient
# of intelligence (c) with the data of the COMRECT.csv file.
# Apart from estimating the parameters and making the tests of each parameter (the parameter is zero 
# or no?) also contrast the appropriate tests to answer the following questions:
# a)The two lines are the same or not?
# b)The two lines are parallel or not?
# c)The two lines have the same independent term or not?
# d)For each of the values of c: 90, 105 and 120 ¾ what differences are there in the valuation, 
# according to the method? Repeat the exercise, but with the valuations (vv).


dd<-read.csv2("./data/COMRECT.csv")
dd$M<-as.factor(dd$M)
#options(contrasts = c("contr.sum", "contr.treatment"))
options(contrasts = c("contr.helmert", "contr.treatment"))
library(lsmeans)
library(car)
names(dd)

head(dd)

#Parallel lines
with(data=dd,scatterplot(C,V,group=M,smooth=F))

#We are going to pose a complete model
mv<-lm(V~M+C+M:C,dd)
summary(mv)


plot(fitted(mv),resid(mv))
abline(h=0,lty=2)
#plot(mv,ask=F)
Anova(mv,ty=3)

# Multiple comparisons at different points of the coefficient
(lsm<-lsmeans(mv,~M|C,at=list(C=c(90,105,120))))
plot(lsm)

#Tests couple by couple
#For these three values of the coefficient, the difference is significant
pairs(lsm)

# Comparison of the slopes
# We see that they are not different
(lsmp<-lsmeans(mv,~M,trend="C"))

# We compare couples. We see the same thing: they are not significant
pairs(lsmp)

#Se there is no interaction. I want to see the best method
mv2<-lm(V~M+C,dd)
plot(fitted(mv2),resid(mv2))
abline(h=0,lty=2)


#plot(mv2,ask=F)
Anova(mv2,ty=3)
(lsm2<-lsmeans(mv2,~M|C,at=list(C=c(90,105,120))))
pairs(lsm2)


with(data=dd,scatterplot(C,VV,group=M,smooth=F))
abline(v=c(90,100,110,120,130),lty=2,col="blue")
mvv<-lm(VV~M+C+M:C,dd)
plot(fitted(mvv),resid(mvv))
abline(h=0,lty=2)
#plot(mvv,ask=F)
Anova(mvv,ty=3)
(lsmvv<-lsmeans(mvv,~M|C,at=list(C=c(90,100,110,120,130))))
pairs(lsmvv)
(lsmpvv<-lsmeans(mvv,~M,trend="C"))
pairs(lsmpvv)
