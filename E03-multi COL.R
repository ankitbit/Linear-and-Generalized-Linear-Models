COL <- read.csv2("./Dades/COL.csv")
p<-4
n<-dim(COL)[1]
library(car)


# MODELO
mod<-lm(C~P+E+H,COL)

# RESUMEN DEL MODELO
summary(mod)

#Calculos opcionales: Intervalos de confiança de los parametros
confint(mod,level=0.99)

#Calculos opcionales: SS1 Test de los parametros con ordenacions predeterminada
anova(mod)
anova(lm(C~H+P+E,COL))

#Nota: SS3, los tests (F) siempre coinciden con los del resumen (t), F=t^2
Anova(mod,ty=3)

# Diagnostico: TENDENCIAS
plot(predict(mod),resid(mod))
abline(h=0,lty=2)

# Diagnostico: OUTLIERS (rstudent)
plot(rstandard(mod))
abline(h=c(-2,0,2),lty=2)

plot(rstudent(mod),main="rstudent")
abline(h=c(-2,0,2),lty=2)

# Diagnostico: LEVERAGE
plot(hatvalues(mod))
abline(h=c(0,2*mean(hatvalues(mod))),lty=2)

# Diagnostico: INFLUENCIA (dffits)
plot(cooks.distance(mod))
abline(h=c(0,4/n),lty=2)

plot(dffits(mod),main="dffits")
abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)


#Diagnosticos de R
oldpar <- par( mfrow=c(2,2))
plot(mod,ask=F)
par(oldpar)

#Diagnosticos: Colinealidad
vif(mod)

# Recordamos RESUMEN DEL MODELO
summary(mod)

#Calculos opcionales: Intervalos de confiança de los parametros
confint(mod,level=0.99)

#Calculos opcionales: Para algunos casos predeterminados IC de E(Y)
(C0<-data.frame(cbind(P=c(65,75,65),E=c(15,15,12),H=c(150,150,150)), row.names=1:3))
predict(mod, C0, interval="confidence", level=.95, se.fit=T)

#Calculos opcionales: Para algunos casos predeterminados I:Prdiccion de Y
predict(mod, C0, interval="prediction", level=.95, se.fit=F)

#Calculos opcionales: SS1 Test de los parametros con ordenacions predeterminada
anova(mod)
#Nota: SS3, los tests (F) coinciden con los del resumen (t), F=t^2
Anova(mod,ty=3)

#Canvios lineales en la variables independiente:
#centrar los datos, solo canvia el termino independiente
summary(lm(C~I(P-mean(P))+I(E-mean(E))+I(H-mean(H)),COL))
# o bien, si P=65, E=15 y H=150 son proximos a las medias:
summary(lm(C~I(P-65)+I(E-15)+I(H-150),COL))

#Canvios lineales en la variables independiente:
#canvio en alguna variable, por ejemplo, exceso de peso, 
# peso patron 0.5*H-10, EP=P-(0.5*H-10)
summary(mod2<-lm(C~I(P-0.5*H+10)+E+H,COL))
vif(mod2)
#Nota: Solo canvia algun parametro y la colinealidad

#Canvios lineales en la variables independiente:
#eliminar alguna variable independiente no significativa y/o con mucha colinealidat
#por ejemplo H, si ya se utiliza el exceso de peso
summary(mod3<-lm(C~I(P-0.5*H+10)+E,COL))
vif(mod3)
