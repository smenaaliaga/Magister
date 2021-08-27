setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 2 - 2021")

# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("corrplot")
# install.packages("validate")
# install.packages("likert")

library(validate)
library(tidyverse)
library(reshape2)
library(corrplot)
library(VIM)
library(likert)
library(plyr)


#############################
## TRANSFORMACION DE DATOS ##
#############################

original <- read.csv2("Resultados Ambulatorio.csv")

summary(factor(original$Frecuencia))

summary(factor(original$Ã.rea))

# X5  : Clínica X posee una diversa gama de convenios y descuentos para 
#       instituciones (empresas, universidades, etc.) y pacientes en general.
# X8  : En Clínica X, el precio de sus servicios es acorde con la calidad de 
#       las prestaciones ofrecidos.
# X14 : Clínica X ofrece cobertura a pacientes afiliados a FONASA.
# X18 : Clínica X cuenta con un sitio web constantemente actualizado, de fácil 
#       navegación y que ofrece múltiples productos y servicios.
#

likert_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

data <- original %>% 
  dplyr::select(
    sexo = Sexo,
    rango = Rango.edad,
    frecuencia = Frecuencia,
    area = Ã.rea,
    X5 = X5, X8 = X8, X14 = X14, X18 = X18
  ) %>% mutate(
    sexo = as.factor(sexo),
    X5 = factor(X5, levels = likert_levels), 
    X8 = factor(X8, levels = likert_levels), 
    X14 = factor(X14, levels = likert_levels), 
    X18 = factor(X18, levels = likert_levels)
  ) 

data$rango <- fct_collapse(data$rango,
                           "[18, 30[" = "Entre 18 y 30 aÃ±os",
                           "[30, 60[" = "Entre 30 y 60 aÃ±os", 
                           "< 18"  = "Menor a 18 aÃ±os", 
                           ">= 60" = "Sobre 60 aÃ±os")

data$rango <- factor(data$rango, 
                     levels = c("< 18", "[18, 30[", "[30, 60[", ">= 60"))

data$frecuencia <- fct_collapse(data$frecuencia,
              "Primera visita" = "Primera visita",
              "Una vez por semana" = c("una vez por semana", 
                                       "Una vez por semana"), 
              "Una vez al mes"  = c("una vez por mes", "Una vez por mes", 
                                    "una vez al mes"), 
              "Una vez al año" = "Una vez al aÃ±o")
  
data$frecuencia <- factor(data$frecuencia, 
                          levels = c("Primera visita", "Una vez al año",
                                     "Una vez al mes", "Una vez por semana"))


data$area <- fct_collapse(data$area,
                      "Consultas" = "Consultas",
                      "Imagenologia" = "ImagenologÃ­a", 
                      "Procedimientos"  = "Procedimientos", 
                      "Urgencia" = "Urgencia",
                      "UTM" = "UTM",
                      "Vacunatorio" = "Vacunatorio")

data$area <- factor(data$area, 
                          levels = c("Consultas", "Imagenologia",
                                     "Procedimientos", "Urgencia",
                                     "UTM", "Vacunatorio"))

summary(data$area)

####################################
## ANALISIS EXPLORATORIO DE DATOS ##

#### SEXO

summary(data$sexo)

ggplot(data, aes(sexo),) +
  geom_bar(size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Sexo") + ylab("Cantidad")


#### RANGO DE EDAD

summary(data$rango)

ggplot(data, aes(rango),) +
  geom_bar(size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Rangos edad") + ylab("Cantidad")


##### SEXO + RANGO EDAD

ggplot(data = data,
       mapping = aes(x = sexo, fill = rango)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Sexo") + ylab("Cantidad") +
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

##### FRECUENCIA

summary(data$frecuencia)

ggplot(data, aes(frecuencia),) +
  geom_bar(size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Frecuencia") + ylab("Cantidad")


##### RANGO + FRECUENCIA

ggplot(data = data,
       mapping = aes(x = rango, fill = frecuencia)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Sexo") + ylab("Cantidad") +
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)


##### LIKERTS X5, X8, X14, X1
# ?likert.bar.plot

summary(data[, 5:8])

lkts <- likert(data[, 5:8, drop = F])

likert.bar.plot(lkts, type = "bar", centered = TRUE) + theme_minimal()

# sexo

lkts_sexo <- likert(data[, 5:8, drop = F], grouping = data$sexo)

likert.bar.plot(lkts_sexo, type = "bar", centered = TRUE) + theme_minimal()

# rango edad

lkts_rango <- likert(data[, 5:8, drop = F], grouping = data$rango)

likert.bar.plot(lkts_rango, type = "bar", centered = TRUE, ordered=F) + 
  theme_minimal()

# frecuencia

summary(data$frecuencia)
summary(frecuencia_sin_NA[, 5:8])

frecuencia_sin_NA <- data[!is.na(data$frecuencia), ]

lkts_frecuencia <- likert(frecuencia_sin_NA[, 5:8, drop = F], 
               grouping = frecuencia_sin_NA$frecuencia)

likert.bar.plot(lkts_frecuencia, type = "bar", centered = TRUE) + 
  theme_minimal()

# area 

summary(data$area)
summary(area_sin_NA[, 5:8])

area_sin_NA <- data[!is.na(data$area), ]

lkts_area <- likert(area_sin_NA[, 5:8, drop = F], grouping = area_sin_NA$area)

likert.bar.plot(lkts_area, type = "bar", centered = TRUE) + theme_minimal()


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
  

