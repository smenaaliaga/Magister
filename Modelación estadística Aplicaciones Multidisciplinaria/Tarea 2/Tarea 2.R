setwd("G:/Mi unidad/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 2")

#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("corrplot")
#install.packages("validate")

library(validate)
library(tidyverse)
library(reshape2)
library(corrplot)
library(VIM)


var <- sample(c(4,5,6,7,8,), prob=c(.2,.3,..,.1), size=1)

#############################
## TRANSFORMACION DE DATOS ##
#############################

data <- read.csv2("resultados.csv")

xs <- data %>% select(X5,X8,X14,X18)
xs_fact <- melt(xs) 

varObjt <- data %>% select(X5,X8,X14,X18,monto)

####################################
## ANALISIS EXPLORATORIO DE DATOS ##
####################################

#### analisis de X5, X8, 14 y X18
summary(xs)

ggplot(data = xs_fact, 
       mapping = aes(variable, value, colour = variable)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Variables") + ylab("valores de escala") 


#### Analisis de monto
summary(data$monto)
sd(data$monto)

qqnorm(data$monto, pch = 1, frame = FALSE)
qqline(data$monto, col = "steelblue", lwd = 2)

ceiling(1 + 3.322 * log10(length(data$monto)))
ggplot(data, aes(monto)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("$") + ylab("Frecuencia") +
  theme_minimal()


ggplot(data = xs_fact, 
       mapping = aes(variable, value, colour = variable)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Variables") + ylab("valores de escala") 


#####################
## P2: DIAGNOSTICO ##
#####################

#Diagnostico del total de datos perdidos y su relacion con el resto de las variables

# Recibe reglas de validacion de datos según un criterio
# La variable guarda la validación
v5 = validator(data$X5>0 & data$X5<10)
cf5 = confront(data,v5)
summary(cf5)
barplot(cf5)

v8 = validator(data$X8>0 & data$X8<10)
cf8 = confront(data,v8)
summary(cf8)

v14 = validator(data$X14>0 & data$X14<10)
cf14 = confront(data,v14)
summary(cf14)

v18 = validator(data$X18>0 & data$X18<10)
cf18 = confront(data,v18)
summary(cf18)

# ¿Donde estan las inconsistencia?
values(cf5)
which(is.na(values(cf5)))

# Graficamente, ¿Donde hay inconsistencias?
aggr(varObjt, sortComb=TRUE, sortVar=TRUE,only.miss=TRUE, cex.axis=0.6, numbers=TRUE,cex.numbers=0.6)

# Impacto de variables en la densidad de otras

# X5
varObjt <- varObjt %>% select(X5, X8, X14, X8, monto)

pbox(varObjt, cex.numbers=1, numbers=T, cex.axis=1, par(mar=c(2.4,2.4,2.4,2.4)))
# Test K-S para subconjunto de perdidos
kruskal.test(varObjt$X5 ~ !is.na(varObjt$X8))
kruskal.test(varObjt$X5 ~ is.na(varObjt$X14))
kruskal.test(varObjt$X5 ~ is.na(varObjt$X18))
kruskal.test(varObjt$X5 ~ is.na(varObjt$monto))
?kruskal.test

is.na(varObjt$X8)

# X8
varObjt <- varObjt %>% select(X8, X5, X14, X18, monto)

pbox(varObjt, cex.numbers=1, numbers=T, cex.axis=0.8, par(mar=c(2.4,2.4,2.4,2.4)))
# Test K-S para subconjunto de perdidos
kruskal.test(data$X8 ~ is.na(data$X5))
kruskal.test(data$X8 ~ is.na(data$X14))
kruskal.test(data$X8 ~ is.na(data$X18))
kruskal.test(data$X8 ~ is.na(data$monto))

# X14
varObjt <- varObjt %>% select(X14, X8, X5, X18, monto)

pbox(varObjt, cex.numbers=1, numbers=T, cex.axis=0.8, par(mar=c(2.4,2.4,2.4,2.4)))
# Test K-S para subconjunto de perdidos
kruskal.test(data$X14 ~ is.na(data$X5))
kruskal.test(data$X14 ~ is.na(data$X8))
kruskal.test(data$X14 ~ is.na(data$X18))
kruskal.test(data$X14 ~ is.na(data$monto))


# X18
varObjt <- varObjt %>% select(X18, X8, X5, X14, monto)

pbox(varObjt, cex.numbers=1, numbers=T, cex.axis=0.8, par(mar=c(2.4,2.4,2.4,2.4)))
# Test K-S para subconjunto de perdidos
kruskal.test(data$X18 ~ is.na(data$X5))
kruskal.test(data$X18 ~ is.na(data$X8))
kruskal.test(data$X18 ~ is.na(data$X14))
kruskal.test(data$X18 ~ is.na(data$monto))



# MONTO
varObjt <- varObjt %>% select(monto, X5, X8, X14, X18)

pbox(varObjt, cex.numbers=1, numbers=T, cex.axis=0.8, par(mar=c(2.4,2.4,2.4,2.4)))
# Test K-S para subconjunto de perdidos
kruskal.test(data$monto ~ is.na(data$X5))
kruskal.test(data$monto ~ is.na(data$X8))
kruskal.test(data$monto ~ is.na(data$X14))
kruskal.test(data$monto ~ is.na(data$X18))



#############################
## P.3 IMPUTATION DE DATOS ##
#############################
# Como impacta en la variabilidad??
# QQ-PLOT PERMITE NO SOLO VALIDAR MODELOS NORMALES, SI NO QUE OTRAS DISTRIBUCIONES
# Test de K-S permite hacer bondad de ajuste con otras distribuciones
# La idea es generar el valor perdido aleatoriamente con la distribucion de los datos


#install.packages('simputation')
library(simputation)

# Imputar con el promedio con su grupo correspondiente (factor)
monto_imputado <- impute_lm(varObjt, monto~1)
monto_imputado
varObjt

monto_cs_imputacion

monto_cs_imputacion <- data.frame(cbind(varObjt$monto, monto_imputado$monto))
monto_cs_imputacion <- monto_cs_imputacion %>% rename("Sin imputar"=X1, 'Imputado con media'=X2)

summary(monto_cs_imputacion)
sd(monto_cs_imputacion$`Sin imputar`, na.rm=TRUE)
sd(monto_cs_imputacion$`Imputado con media`)

monto_cs_imputacion_fact <- melt(monto_cs_imputacion) 

monto_cs_imputacion_fact

qplot(x=value, geom="density", group=factor(variable), colour=factor(variable),
      fill=factor(variable),alpha=I(.5),data=monto_cs_imputacion_fact) +
  theme_minimal() +
  xlab("$") + ylab("densidad") 


# Imputar con la mediana
X_imputados <- impute_median(varObjt,X5~is.na(data$X14))
summary(X_imputados[,-5])
summary(varObjt[,-5])

is.na(data$X14)


#### Imputacion por grupo de asuencia 
X_imputados <- impute_median(varObjt,X5~1)
summary(X_imputados[,-5])
summary(varObjt[,-5])


#### Imputación aleatoria
rand.imput <-function(x,a,b,c,d,e,f,g,h,i,tot){
  missing <- (is.na(x)) #vector booleano
  n.missing <- sum(missing)#Numero de NA's
  x.obs <- x[!missing]#Datos no NA
  imputed <- x
  imputed[missing] <- sample(c(1,2,3,4,5,6,7,8,9), 
                             prob=c(a/tot,b/tot,c/tot,d/tot,e/tot,
                                    f/tot,g/tot,h/tot,i/tot), 
                             size=1)
  return(imputed)
}

summary(data$X5)
table(data$X5)
complete.X5 <- rand.imput(data$X5)
summary(complete.X5)

summary(data$X8)
table(data$X8)
complete.X8 <- rand.imput(data$X8,0,3,2,8,30,33,55,107,72,310)
summary(complete.X8)

summary(data$X14)
table(data$X14)
complete.X14 <- rand.imput(data$X14,5,14,8,19,41,14,43,36,44,320-96)
summary(complete.X14)

summary(data$X18)
table(data$X18)
complete.X18 <- rand.imput(data$X18,4,4,4,10,19,25,32,79,68,320-75)
summary(complete.X18)

# Imputacion aleatoria de la variable monto
rand.imput <-function(x){
  missing <- (is.na(x)) #vector booleano
  n.missing <- sum(missing)#Numero de NA's
  x.obs <- x[!missing]#Datos no NA
  imputed <- x
  imputed[missing] <- sample(x.obs,n.missing,replace = T)
  #Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
  return(imputed)}

complete.monto <- rand.imput(data$monto)
summary(data$monto)
sd(data$monto, na.rm=TRUE)
summary(complete.monto)
sd(complete.monto)


#############################################
## P.4 IMPACTO EN LA RELACION DE VARIABLES ##
#############################################
#Se puede usar pearson para ver correlación con la variable monto prestacion
# Variables tipo Likert 

# X5
cor.test(data$X5, data$X8, method="spearman")
cor.test(X_imputados$X5, X_imputados$X8, method="spearman")
cor.test(complete.X5, complete.X8, method="spearman")

cor.test(data$X5, data$X14, method="spearman")
cor.test(X_imputados$X5, X_imputados$X14, method="spearman")
cor.test(complete.X5, complete.X14, method="spearman")

cor.test(data$X5, data$X18, method="spearman")
cor.test(X_imputados$X5, X_imputados$X18, method="spearman")
cor.test(complete.X5, complete.X18, method="spearman")


# X8
cor.test(data$X8, data$X5, method="spearman")[4]
cor.test(X_imputados$X8, X_imputados$X5, method="spearman")[4]
cor.test(complete.X8, complete.X5, method="spearman")[4]

cor.test(data$X8, data$X14, method="spearman")[4]
cor.test(X_imputados$X8, X_imputados$X14, method="spearman")[4]
cor.test(complete.X8, complete.X14, method="spearman")[4]

cor.test(data$X8, data$X18, method="spearman")[4]
cor.test(X_imputados$X8, X_imputados$X18, method="spearman")[4]
cor.test(complete.X8, complete.X18, method="spearman")[4]


# X14
cor.test(data$X14, data$X5, method="spearman")[4]
cor.test(X_imputados$X14, X_imputados$X5, method="spearman")[4]
cor.test(complete.X14, complete.X5, method="spearman")[4]

cor.test(data$X14, data$X8, method="spearman")[4]
cor.test(X_imputados$X14, X_imputados$X8, method="spearman")[4]
cor.test(complete.X14, complete.X8, method="spearman")[4]

cor.test(data$X14, data$X18, method="spearman")[4]
cor.test(X_imputados$X14, X_imputados$X18, method="spearman")[4]
cor.test(complete.X14, complete.X18, method="spearman")[4]


# X18
cor.test(data$X18, data$X5, method="spearman")[4]
cor.test(X_imputados$X18, X_imputados$X5, method="spearman")[4]
cor.test(complete.X18, complete.X5, method="spearman")[4]

cor.test(data$X18, data$X8, method="spearman")[4]
cor.test(X_imputados$X18, X_imputados$X8, method="spearman")[4]
cor.test(complete.X18, complete.X8, method="spearman")[4]

cor.test(data$X18, data$X14, method="spearman")[4]
cor.test(X_imputados$X18, X_imputados$X14, method="spearman")[4]
cor.test(complete.X18, complete.X14, method="spearman")[4]



#############################################
## P.5 IMPUTACION POR REGRESION PARA MONTO ##
#############################################

reg_monto = impute_lm(X_imputados,monto~X5+X8+X14+X18)

summary(data$monto)
sd(data$monto, na.rm=TRUE)

summary(monto_imputado$monto)
sd(monto_imputado$monto, na.rm=TRUE)

summary(reg_monto$monto) 
sd(reg_monto$monto, na.rm=TRUE)
  

