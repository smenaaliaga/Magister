# tildes omitidos intencionalmente

# t-test para la media en el caso de 1 poblacion, 1 cola
dat=read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)

# opciones: por defecto es a 2 colas. Para 1 cola se especifica "less" (cola izquierda) o "greater"
t.test(dat$td,mu=31,alternative="less",conf.level=0.95) 

# t-test para la media en el caso de 1 poblacion, 2 colas
t.test(dat$td,mu=31,conf.level=0.95) 

# test de chi cuadrado para la varianza
install.packages("EnvStats")
library(EnvStats)
dat=read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)
varTest(dat$kg,sigma.squared=0.12, alternative="greater")

# sobre la distribucion normal # ===== #

install.packages("moments")
library(moments)

skewness(dat$kg)
kurtosis(dat$kg) #referencia de la distribucion normal = 3

## _____ ##
## Prueba de Chi Cuadrado de Independencia ##
datos=read.csv(file.choose(),header=T,sep=";",dec=",")
head(datos)

# tabla de doble entrada en R:
tabla=table(datos$Ing,datos$Gen)
tabla

# tabla con las proporciones:
prop.table(tabla)
prop.table(tabla,1)
prop.table(tabla,2)

# chi cuadrado para independencia:
summary(tabla)


# binom.test # ===== #
binom.test(8,20,0.25,alternative = "greater")

