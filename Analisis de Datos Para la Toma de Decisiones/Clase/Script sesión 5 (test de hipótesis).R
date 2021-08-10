#-----------------------------------
# Funciones para decidir el test
#-----------------------------------
#E1
#valor critico
qt(0.05,df=22,lower.tail=T)

#E2
#valor critico
qnorm(0.05/2,mean=0,sd=1)
qnorm(0.07/2,mean=0,sd=1)

#E3
#valor critico
qchisq(0.95,44) #1-alpha
qchisq(0.05,44,lower.tail=F) #alpha

#p-values
#========
pt(1.283,df=22,lower.tail=T)
2*pnorm(-0.298,mean=0,sd=1)
1-pchisq(23.674,df=44,lower.tail=T)
#alternativa al anterior:
pchisq(23.674,df=44,lower.tail=F)

#-----------------------------------
# Normalidad
#-----------------------------------
#Shapiro
ejemplo=rnorm(n=23,mean=40,sd=13)

hist(ejemplo)

qqnorm(ejemplo, pch = 1, frame = FALSE)
qqline(ejemplo, col = "steelblue", lwd = 2)

shapiro.test(ejemplo)

#opcion 2:
install.packages("car") #primero instalar
library(car)
qqPlot(ejemplo)

#KS
ejemplo2=rnorm(n=67,mean=43,sd=14)

qqnorm(ejemplo2, pch = 1, frame = FALSE)
qqline(ejemplo2, col = "steelblue", lwd = 2)

qqPlot(ejemplo2)

ks.test(ejemplo2,"pnorm",mean=mean(ejemplo2),sd=sd(ejemplo2))

