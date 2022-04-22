# setear ubicación del proyecto
setwd('Documents/Magister/Metodos Multivariados, Clasificacion y Pronostico/Tarea 1')
# Librerias
# install.packages("tidyverse")
# install.packages("rlang")
# install.packages('stringi')
library(tidyverse)
# Exportacion de datos
data = read.csv("churn-analysis.csv", header = TRUE, sep=";")
# tipos de datos en df
str(data)
# Transformacion de datos character a factor
data <- data %>% mutate_if(is.character, as.factor)
# Resumen estadistico
summary(data)
