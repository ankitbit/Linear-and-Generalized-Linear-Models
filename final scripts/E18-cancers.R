#setwd("~/Documents/CURS 2017-2018/MLLG/Pràctiques")
dd<-read.csv2("./data/clusters.csv")
library(car)
scatterplot( dd$Distance, dd$Cancers, smooth = F)
#scatterplot( log( dd$Distance ), dd$Cancers, smooth = F)
m1<-glm( Cancers ~ Distance, family = poisson, data = dd )
summary( m1 )

# To calculate the dispersion parameter (phi)
# To calculate pearson statistics
(PS<-sum(residuals(m1,type="pearson")^2))
(phi = PS/m1$df.res)

# P-value that this would have, pk comes from a chi square
2*min(pchisq(PS,m1$df.res),1-pchisq(PS,m1$df.res))
c(qchisq(0.025,m1$df.res),qchisq(0.975,m1$df.res))/m1$df.res

#It is not significant and there is about dispersion
plot(dd$Distance,rstandard(m1,type='pearson'))
abline(h=c(-3,-2,0,2,3),lty=2)
plot(dd$Distance,rstudent(m1,type='pearson')) 
abline(h=c(-3,-2,0,2,3),lty=2)
plot(m1,which=1)

m2<-glm(Cancers~Distance,family=poisson(link=identity),data=dd)
summary(m2)
(PS2<-sum(residuals(m2,type="pearson")^2))
PS2/m2$df.res
2*min(pchisq(PS2,m2$df.res),1-pchisq(PS2,m2$df.res))
c(qchisq(0.025,m2$df.res),qchisq(0.975,m2$df.res))/m1$df.res
plot(dd$Distance,rstandard(m2,type='pearson'))
abline(h=c(-3,-2,0,2,3),lty=2)
plot(dd$Distance,rstudent(m2,type='pearson'))
abline(h=c(-3,-2,0,2,3),lty=2)
plot(m2,which=1)

plot(dd$Distance,dd$Cancers,xlim=c(0,250),ylim=c(-1,6),pch=3,cex=.7)
lines(0:250,exp(coef(m1)[1]+coef(m1)[2]*(0:250)),col="blue")
lines(0:250,(coef(m2)[1]+coef(m2)[2]*(0:250)),col="red")
abline(h=0,col="grey")
abline(v=0,col="grey")

# Opcional

mq<-glm(Cancers~Distance,family=quasipoisson,data=dd)
summary(mq)
(PSq<-sum(residuals(mq,type="pearson")^2))
PSq/mq$df.res
plot(dd$Distance,rstandard(mq,type='pearson'))
abline(h=c(-3,-2,0,2,3),lty=2)
plot(dd$Distance,rstudent(mq,type='pearson'))
abline(h=c(-3,-2,0,2,3),lty=2)
plot(mq,which=1)


