# Librerias
library(AMR)
library(fdth)
library(tidyverse)

# Opciones
options(digits=3)

var_hist <- function(array) {
  
  # Valores necesarios para histograma
  var_hist <- vector(mode="list", length=5)
  names(var_hist) <- c("LI", "LS", "R", "K", "A")
  
  # Limite inferior
  var_hist$LI = min(array) - 0.5*0.1
  # Limite superior
  var_hist$LS = max(array)
  # Rango (otra forma: diff(range(TotalUSD))
  var_hist$R = var_hist$LS - var_hist$LI
  # Numero de Clases : Regla de Sturges 
  var_hist$K <- 3.3*log10(length(TotalUSD)) + 1
  # Amplitud
  var_hist$A = round((diff(range(TotalUSD)) + 0.01)/var_hist$K)
  
  return(var_hist)
}

setwd("C:/Users/smena/Google Drive/Central de proyecto e ideas/Magister/Tarea")

# Obtención de datos
base <- read.csv2(file.choose())

# Ver datos
head(base)
names(base)(base[-1])

# Atajar variable a utilizar
attach(base)

# Resumen estadistico sin ID
summary(TotalUSD)

# Obtener variables del histograma TotalUSD
varhist <- var_hist(TotalUSD)

# Obtener las clases
class <- seq(from=varhist$LI, to=varhist$LS+14, by=varhist$A)
class

varhist$LI
varhist$LS
varhist$R
varhist$K
varhist$A

ggplot(base, aes(TotalUSD)) +
  geom_histogram(bins=round(varhist$K), breaks=class) +
  xlab("USD") + ylab("Número de clientes") +
  ggtitle("Total de USD de clientes en tarjeta de credito") +
  scale_x_continuous(breaks = class)  + 
  theme_minimal() 

# Grafico de Ojiva

?table
table(TotalUSD,breaks=class)


marca <- c(13.2+49.2,49.2+85.2,85.2+121.2,121.2+157.2,157.2+193.2,193.2+229.2,229.2+265.2,265.2+301.2)/2
marca

freqRel <- c(0.36, 0.69, 0.84, 0.91, 0.95, 0.96, 0.98, 1)

oji <- data.frame(marca, freqRel)

ggplot(data=oji, aes(x=oji$marca, y=oji$freqRel)) +
  geom_line() + geom_point() +
  xlab("USD") + ylab("Frecuencia Relativa") +
  scale_x_continuous(breaks = oji$marca)  + 
  theme_minimal() 

# Preunta 2

ggplot(data = base, aes(x=TipoCliente, y=TotalUSD)) + 
  geom_boxplot(aes(fill=TipoCliente)) +
  xlab("Tipo de Cliente") + ylab("USD") +
  theme_minimal() 

summary(base$TipoCliente)


#############
# Sección 2 #
#############

# Pregunta 1

?qplot
# Simulación de una chi cuadrado
chisqr7 <- rchisq(10000, df=7)
qplot(chisqr7, geom="histogram") +
  theme_minimal() 
summary(chisqr7)
sd(chisqr7)

chisqr22 <- rchisq(10000, df=22)
qplot(chisqr22, geom="histogram") +
  theme_minimal() 
summary(chisqr22)
sd(chisqr22)

# Simulación a partir de una distribución normal estandar

chisqrNrom7 <- c(1:10000)
for(i in 1:10000){
  chisqrNrom7[i] <- 0
  for(j in 1:7){
    chisqrNrom7[i] <- chisqrNrom7[i] + rnorm(1, mean=0, sd=1)^2
  }
}
qplot(chisqrNrom7, geom="histogram") +
  theme_minimal() 
summary(chisqrNrom7)
sd(chisqrNrom7)

chisqrNrom22 <- c(1:10000)
for(i in 1:10000){
  chisqrNrom22[i] <- 0
  for(j in 1:22){
    chisqrNrom22[i] <- chisqrNrom22[i] + rnorm(1, mean=0, sd=1)^2
  }
}
qplot(chisqrNrom22, geom="histogram") +
  theme_minimal() 
summary(chisqrNrom22)
sd(chisqrNrom22)

# Comparar: grafico, dispersión, posición

# Pregunta 2

rt1 <- rt(10000, df=4)
qplot(rt1, geom="density") +
  theme_minimal() 
summary(rt1)
sd(rt1)

rt2 <- rt(10000, df=10)
qplot(rt2, geom="density") +
  theme_minimal() 
summary(rt2)
sd(rt2)

rt3 <- rt(10000, df=22)
qplot(rt3, geom="density") +
  theme_minimal() 
summary(rt3)
sd(rt3)


sdtest <- c(1:10000)
for( i in 1:10000){
  sdtest[i] <- sd(rnorm(10000,0,1))
}
max(sdtest)


sdtest2 <- c(1:10000)
for( i in 1:10000){
  sdtest2[i] <- sd(rt(10000,7))
}
min(sdtest)


summary(rnorm(10000,0,1))


#############
# Sección 3 #
#############

##### Parte uno

#Normalización
3/(19/sqrt(57))

# Probabilidad
pnorm(1.192) - pnorm(-1.192)

##### Parte dos
harina <- rnorm(57, 976, 19)
qplot(harina, geom="density") +

  theme_minimal() 
summary(harina)

normalize <- c(1:57)
for(i in 1:57){
  normalize[i] <- (harina[i]-mean(harina))/(19/sqrt(57))
}


?geom_vline
qplot(normalize, geom="density") +
  theme_minimal() +
  geom_vline(xintercept = 1.192, linetype="dashed", color = "red", show.legend=TRUE)  +
  geom_vline(xintercept = -1.192, linetype="dashed", color = "red") +
  geom_text(aes(x=3.5, label="1.192", y=0.005), colour="red", text=element_text(size=11)) +
  geom_text(aes(x=-3.5, label="-1.192", y=0.005), colour="red", text=element_text(size=11))

summary(normalize)


filtered1 <- normalize[(normalize > 1.192079)]

filtered2 <- normalize[(normalize < -1.192079)]

n2 <- length(filtered1) + length(filtered2)

n2 / 57
    
  
