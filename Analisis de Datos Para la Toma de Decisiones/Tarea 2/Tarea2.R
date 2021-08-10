library(tidyverse)
library(moments)
library(car)

#############
## TAREA 2 #
############

#### Pregunta 1 ####
dat <- read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)

attach(dat)

summary(RM)

## Parte 1
ceiling(1 + 3.322 * log10(length(RM)))
ggplot(dat, aes(RM)) +
  geom_histogram(bins = 8, color="white") +
  xlab("kg/cm2") + ylab("Frecuencia") +
  ggtitle("Resistencia media de compresión en cerámicas") +
  theme_minimal() 

ggplot(dat, aes(x="", y=RM)) + 
  geom_boxplot() +
  xlab("Cerámica") + ylab("kg/cm2") +
  ggtitle("Resistencia media de compresión en cerámicas") +
  theme_minimal() 

qqPlot(RM)

qqnorm(RM, pch = 1, frame = FALSE)
qqline(RM, col = "steelblue", lwd = 2)

skewness(RM)
kurtosis(RM)

ks.test(RM,pnorm,mean(RM),sd(RM))

# Parte 2

# Ho <=  5040 [kg/cm2]
# H1 >  5040 [kg/cm2]

Zobs <- (mean(RM) - 5040) * sqrt(30) / sd(RM)
qnorm(1-0.07,mean=0,sd=1)
Zobs

1-pnorm(Zobs)

qnorm(RM,sd)

#valores al azar de la distribución normal
randNorm <- rnorm(3000)
#calculo de su densidad
randDensity <- dnorm(randNorm)

ggplot(data.frame(x = randNorm, y = randDensity)) + 
  aes(x = x, y = y) +
  geom_point(size = 1) + 
  labs(x = "Normal Estandar", y = "Densidad") +
  theme_minimal() 
1-0.07

# Parte 3
t.test(RM,mu=5040,alternative="greater",conf.level=0.93) 

# Parte 4
Zobs <- (mean(RM) - 5050) * sqrt(30) / sd(RM)
qnorm(1-0.05,mean=0,sd=1)



#### Pregunta 2 ####
dat <- read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)

attach(dat)

summary(C1)

## Parte 1
ceiling(1 + 3.322 * log10(length(C1)))
ggplot(dat, aes(C1)) +
  geom_histogram(bins = 5, color="white") +
  xlab("Cantidad") + ylab("Frecuencia") +
  ggtitle("Bicicletas diarias solicidas por centro público") +
  theme_minimal() 

ggplot(dat, aes(x="", y=C1)) + 
  geom_boxplot() +
  xlab("Bicicletas") + ylab("Cantidad") +
  ggtitle("Bicicletas diarias solicidas por centro público") +
  theme_minimal() 

qqnorm(C1, pch = 1, frame = FALSE)
qqline(C1, col = "steelblue", lwd = 2)

skewness(C1)
kurtosis(C1)

shapiro.test(C1)

## Parte 2
Tobs <- (mean(C1) - 83) * sqrt(25) / sd(C1)
qt(0.15,24,lower.tail=F)|

## Parte 3
qt(0.05,24,lower.tail=F)



#### Pregunta 3 ####
qnorm(0.01/2,mean=0,sd=1)
Zobs <- ( (7.6 - 8) * sqrt(50) ) / 0.5


## Parte 2
# Region de rechazo original -2.576 y 2.576
( (7.6 - 9) * sqrt(50) ) / 0.5

-2.576 * 0.5 / ( (7.6 - 100) * sqrt(50) )


?pnorm
2*pnorm(-2.576 * 0.5 / ( (7.6 - 100) * sqrt(50) ))

#### Pregunta 4 ####
Pobs <- ( (8/17) - 0.67 ) / sqrt( ( 0.67 * (1-0.67) ) / 17 )
qnorm(0.1/2)

Pobs <- ( (68/99) - 0.67 ) / sqrt( ( 0.67 * (1-0.67) ) / 17 )
  
