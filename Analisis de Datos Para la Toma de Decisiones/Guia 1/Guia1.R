#########################
## GUIA DE EJERCICIOS 1 #
#########################

#### Pregunta 1 ####
dat <- read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)

attach(dat)

# Normalidad
hist(T_Espera)

qqnorm(T_Espera, pch = 1, frame = FALSE)
qqline(T_Espera, col = "steelblue", lwd = 2)

#opcion 2:
install.packages("car") #primero instalar
library(car)
qqPlot(T_Espera)

shapiro.test(T_Espera)

# t test para la media
# Forma 1
# Calcular estadistico
Zobs <- (mean(T_Espera) - 9.3) * sqrt(30) / sd(T_Espera)
Zcrit <- qnorm(0.05)

# Forma 2 (p-valor)
t.test(T_Espera,mu=29.3,alternative="less",conf.level=0.95) 


#### Pregunta 2 ####

# Forma 1
Xobs <- (30-1)*sd(T_Espera)^2 / 1.69
Xcrit <- qchisq(0.05,29,lower.tail = FALSE)

# Forma 2
install.packages("EnvStats")
library(EnvStats)
varTest(T_Espera,sigma.squared=1.69, alternative="greater")


#### Pregunta 3 ####

dat <- read.csv(file.choose(),header=T,sep=";",dec=",")
head(dat)
summary(dat)
 dat$Comuna <- as.factor(dat$Comuna)
dat$Acceso <- as.factor(dat$Comuna)

comu_nivel <- table(dat$Comuna, dat$Nivel)
comu_nivel

summary(comu_nivel)

comu_ 
