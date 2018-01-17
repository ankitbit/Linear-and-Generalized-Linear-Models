dd<-read.csv2("./data/insecticida.csv")
library(RcmdrMisc)
scatterplot(MORTS/T~log(DOSI), reg.line=lm, smooth=F, spread=F, boxplots=F, span=0.5, data=dd)

scatterplot(qnorm(MORTS/T)~log(DOSI), reg.line=lm, smooth=F, spread=F, 
            boxplots=F, span=0.5, data=dd[2:10,])

m0<-lm(qnorm(MORTS/T)~log(DOSI),data=dd[which(dd$MORTS>0),])
summary(m0)
plot(m0,which=1)
with(dd,plot(log(DOSI),MORTS/T))
with(dd[2:10,],lines(log(DOSI),pnorm(predict(m0))))
plot(predict(m0),resid(m0))
abline(h=0,lty=2)

m1<-glm(cbind(MORTS,T-MORTS)~log(DOSI),family=binomial(link="probit"),data=dd)
summary(m1)

plot(m1,which=1)
plot(log(dd$DOSI),dd$MORTS/dd$T)
lines(log(dd$DOSI),predict(m1,type="response"))
plot(log(dd$DOSI),resid(m1,type="pearson"))
abline(h=0,lty=2)

pd<-(0:100)
pih<-predict(m1,data.frame(DOSI=pd),type="response",se.fit=T)
plot(dd$DOSI,dd$MORTS/dd$T,ylim=c(0,1))
lines(pd,pih$fit)
lines(pd,pih$fit+1.96*pih$se,col="red")
lines(pd,pih$fit-1.96*pih$se,col="red")
abline(h=c(0,1),lty=2)

pd<-exp(seq(from=min(log(dd$DOSI)),to=max(log(dd$DOSI)),length.out = 50))
pih<-predict(m1,data.frame(DOSI=pd),type="response",se.fit=T)
plot(log(dd$DOSI),dd$MORTS/dd$T,ylim=c(0,1))
lines(log(pd),pih$fit)
lines(log(pd),pih$fit+1.96*pih$se,col="red")
lines(log(pd),pih$fit-1.96*pih$se,col="red")
abline(h=c(0,1),lty=2)


m2<-glm(cbind(MORTS,T-MORTS)~log(DOSI),family=binomial(link="logit"),data=dd)
summary(m2)
sum(residuals(m2,type="pearson")^2)
plot(m2,which=1)
plot(log(dd$DOSI),dd$MORTS/dd$T)
lines(log(dd$DOSI),predict(m2,type="response"))
plot(log(dd$DOSI),resid(m2,type="pearson"))
abline(h=0,lty=2)

#To calculate the dispersion parameter (phi)
(PS<-sum(residuals(m2,type="pearson")^2))
(phi = PS/m2$df.res)

# P-value that this would have, pk comes from a chi square
2*min(pchisq(PS,m2$df.res),1-pchisq(PS,m2$df.res))
c(qchisq(0.025,m2$df.res),qchisq(0.975,m2$df.res))/m2$df.res



m3<-glm(cbind(MORTS,T-MORTS)~log(DOSI),family=binomial(link="cloglog"),data=dd)
summary(m3)
sum(residuals(m3,type="pearson")^2)
sum(residuals(m3,type="deviance")^2)/m3$df.residual
plot(m3,which=1)
plot(log(dd$DOSI),dd$MORTS/dd$T)
lines(log(dd$DOSI),predict(m3,type="response"))
plot(log(dd$DOSI),resid(m3,type="pearson"))
abline(h=0,lty=2)

m4<-glm(cbind(MORTS,T-MORTS)~log(DOSI),family=quasibinomial(link="cloglog"),data=dd)
summary(m4)
sum(residuals(m4,type="pearson")^2)/m4$df.residual
sum(residuals(m4,type="deviance")^2)/m4$df.residual
plot(m4,which=1)
plot(log(dd$DOSI),dd$MORTS/dd$T)
lines(log(dd$DOSI),predict(m4,type="response"))
plot(log(dd$DOSI),resid(m4,type="pearson"))
abline(h=0,lty=2)

#glm(cbind(MORTS,T-MORTS)~log(DOSI),family=binomial(link="identity"),data=dd)

m1$deviance
m2$deviance
m3$deviance
m4$deviance

m1$deviance/m1$df.residual
m2$deviance/m2$df.residual
m3$deviance/m3$df.residual
m4$deviance/m4$df.residual

logLik(m1)
logLik(m2)
logLik(m3)
logLik(m4)


# this would be the equivalent to R ^ 2
1-m1$deviance/m1$null.deviance
1-m2$deviance/m2$null.deviance
1-m3$deviance/m3$null.deviance
1-m4$deviance/m4$null.deviance

