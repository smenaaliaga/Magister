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
#    area = Ã.rea,
    X5 = X5, X8 = X8, X14 = X14, X18 = X18,
    A = A, B = B, C = C
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


#data$area <- fct_collapse(data$area,
#                      "Consultas" = "Consultas",
#                      "Imagenologia" = "ImagenologÃ­a", 
#                      "Procedimientos"  = "Procedimientos", 
#                      "Urgencia" = "Urgencia",
#                      "UTM" = "UTM",
#                      "Vacunatorio" = "Vacunatorio")

#data$area <- factor(data$area, 
#                          levels = c("Consultas", "Imagenologia",
#                                     "Procedimientos", "Urgencia",
#                                     "UTM", "Vacunatorio"))

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

?aggr
# Graficamente, ¿Donde hay inconsistencias?
aggr(data,numbers=T,sortVar=T)

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

# Impacto de variables en la densidad de otras

# X5
likerts <- data %>% dplyr::select(X5, X8, X14, X18)

?pbox
pbox(likerts, cex.axis=0.8)

# X8
likerts <- data %>% dplyr::select(X8, X5, X14, X18)

pbox(likerts, cex.numbers=1, numbers=T, cex.axis=0.8)

# X14
likerts <- data %>% dplyr::select(X14, X8, X5, X18)

pbox(likerts, cex.numbers=1, numbers=T, cex.axis=0.8)

# X18
likerts <- data %>% dplyr::select(X18, X5, X8, X14)

pbox(likerts, cex.numbers=1, numbers=T, cex.axis=0.8)
  


#############################
## P.3 IMPUTATION DE DATOS ##
#############################

# install.package("simputation")
library(simputation)

# Se convierten valores likerts (factors) a numericos
likerts$X5 <- as.numeric(likerts$X5)
likerts$X8 <- as.numeric(likerts$X8)
likerts$X14 <- as.numeric(likerts$X14)
likerts$X18 <- as.numeric(likerts$X18)


#### Se realiza la imputación por mediana
?impute_median
data$X5 <- as.numeric(data$X5)
median(as.numeric(data$X5), na.rm = T)
median_imputation <- impute_median(likerts, X5+X8+X14+X18~1)
summary(median_imputation)
summary(likerts)


#### Revisión de distribución

## X5

imputation_x5 <- data.frame(likerts$X5, median_imputation$X5)
imputation_x5_melt <- melt(imputation_x5)

ggplot(data = imputation_x5_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X8

imputation_x8 <- data.frame(likerts$X8, median_imputation$X8)
imputation_x8_melt <- melt(imputation_x8)

ggplot(data = imputation_x8_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X14

imputation_x14 <- data.frame(sin_imputacion = likerts$X14, con_imputacion = median_imputation$X14)
imputation_x14_melt <- melt(imputation_x14)

ggplot(data = imputation_x14_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X18

imputation_x18 <- data.frame(sin_imputacion = likerts$X18, con_imputacion = median_imputation$X18)
imputation_x18_melt <- melt(imputation_x18)

ggplot(data = imputation_x18_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())





#### Se realiza la imputación por mediana agrupado por grupo etario y frecuencia

# Se convierten valores likerts (factors) a numericos
data$X5 <- as.numeric(data$X5)
data$X8 <- as.numeric(data$X8)
data$X14 <- as.numeric(data$X14)
data$X18 <- as.numeric(data$X18)

?impute_median
data$X5 <- as.numeric(data$X5)
median(as.numeric(data$X5), na.rm = T)
median_imputation_grouped <- impute_median(data, X5+X8+X14+X18~frecuencia)
summary(median_imputation_grouped)
summary(likerts)


#### Revisión de distribución

## X5

imputation_x5 <- data.frame(likerts$X5, median_imputation_grouped$X5)
imputation_x5_melt <- melt(imputation_x5)

ggplot(data = imputation_x5_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X8

imputation_x8 <- data.frame(likerts$X8, median_imputation_grouped$X8)
imputation_x8_melt <- melt(imputation_x8)

ggplot(data = imputation_x8_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X14

imputation_x14 <- data.frame(likerts$X14, median_imputation_grouped$X14)
imputation_x14_melt <- melt(imputation_x14)

ggplot(data = imputation_x14_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

summary(median_imputation_grouped$X14)
sd(median_imputation_grouped$X18)
summary(data$X14)
sd(data$X18, na.rm = T)

## X18

imputation_x18 <- data.frame(likerts$X18, median_imputation_grouped$X18)
imputation_x18_melt <- melt(imputation_x18)

ggplot(data = imputation_x18_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())





#### IMPUTACION POR METODO HOT-DECK

library(VIM)

# Definimos un dataframe auxiliar para no perder la variable original
hot_deck <- hotdeck(likerts)

# Verificamos que no existen faltantes
sum(is.na(hot_deck$X14))

## X5
hot_deck_x5 <- data.frame(likerts$X5, hot_deck$X5)
hot_deck_x5_melt <- melt(hot_deck_x5)

ggplot(data = hot_deck_x5_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## X8

hot_deck_x8 <- data.frame(likerts$X8, hot_deck$X8)
hot_deck_x8_melt <- melt(hot_deck_x8)

ggplot(data = hot_deck_x8_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X14

hot_deck_x14 <- data.frame(likerts$X14, hot_deck$X14)
hot_deck_x14_melt <- melt(hot_deck_x14)

ggplot(data = hot_deck_x14_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

summary(median_imputation_grouped$X14)
sd(median_imputation_grouped$X18)
summary(data$X14)
sd(data$X18, na.rm = T)

## X18

hot_deck_x18 <- data.frame(likerts$X18, hot_deck$X18)
hot_deck_x18_melt <- melt(hot_deck_x18)

ggplot(data = hot_deck_x18_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())





#### IMPUTACION POR METODO REGRESION LINEAL

data$X5 <- as.numeric(data$X5)
data$X8 <- as.numeric(data$X8)
data$X14 <- as.numeric(data$X14)
data$X18 <- as.numeric(data$X18)
data$A <- as.numeric(data$A)
data$B <- as.numeric(data$B)
data$C <- as.numeric(data$C)

regression <- impute_lm(data, X5+X8+X14+X18~A+B+C)
summary(regression)
summary(likerts)




## X5
regression_x5 <- data.frame(likerts$X5, ceiling(regression$X5))
regression_melt <- melt(regression_x5)

ggplot(data = regression_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## X8

regression_x8 <- data.frame(likerts$X8, ceiling(regression$X8))
regression_x8_melt <- melt(regression_x8)

ggplot(data = regression_x8_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## X14

regression_x14 <- data.frame(likerts$X14, ceiling(regression$X14))
regression_x14_melt <- melt(regression_x14)

ggplot(data = regression_x14_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

summary(median_imputation_grouped$X14)
sd(median_imputation_grouped$X18)
summary(data$X14)
sd(data$X18, na.rm = T)

## X18

regression_x18 <- data.frame(likerts$X18, ceiling(regression$X18))
regression_x18_melt <- melt(regression_x18)

ggplot(data = regression_x18_melt, 
       mapping = aes(x=value, fill = variable)) +
  geom_bar(position="dodge") +
  theme_minimal() +
  xlab("Escala") + ylab("Frecuencia") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())




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
  

