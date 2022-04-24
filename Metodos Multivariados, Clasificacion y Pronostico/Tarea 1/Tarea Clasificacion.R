# setear ubicación del proyecto
setwd('Documents/Magister/Metodos Multivariados, Clasificacion y Pronostico/Tarea 1')
# Librerias
# install.packages("tidyverse")
# install.packages('xtable')
# install.packages("GGally")
# install.packages('MVN')
library(tidyverse)
library(xtable)
library(GGally)
library(MVN)
# Exportacion de datos
data = read.csv("churn-analysis.csv", header = TRUE, sep=";")
# tipos de datos en df
str(data)
# Transformacion de datos character a factor
data <- data %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(area.code = as.factor(area.code))
## AED ##
#########
# Resumen estadistico todas las variables
summary(data)
# Hay NA?
which(is.na(data))
###### Variable de Respuesta Y : churn
xtable(as.table(
  summary(data$churn)
), type = "latex")
###### Vaeriables explicativas X
### Categoricas 
data_fact <- data %>%
  select_if(is.factor)
# summarys latex
for(i in 1:ncol(data_fact)){
  summaries <-xtable(as.table(
    summary(data_fact[i])
  ), type = "latex")
  print(summaries)
}
### Continuas
data_cont <- data %>%
        select_if(~!is.factor(.))
# summarys latex
for(i in 1:ncol(data_cont)){
  summaries <-xtable(as.table(
    summary(data_cont[i])
  ), type = "latex")
  print(summaries)
}
# ploteo de todas las variables numericas
for (i in 1:ncol(data_cont)) {
  p = ggplot(data_cont, aes(data_cont[,i], fill = data$churn)) +
    geom_histogram(bins = 12, color="white", position = "identity", boundary = 0) +
    xlab(colnames(data_cont)[i]) + ylab("Frecuencia") +
    scale_fill_discrete(name = "churn") +
    theme_minimal() 
  ggsave(p, file=paste0(colnames(data_cont)[i],".pdf"), 
         width = 14, height = 10, units = "cm")
  print(p)
}
# ggparis
ggpairs(data_cont, aes(color = data$churn, alpha = 0.5),
        upper = list(continuous = "points"))
# Outliers
for(i in 1:ncol(data_cont)){
  print(colnames(data_cont)[i])
  print(boxplot.stats(data_cont[,i])$out)
  print(paste0("Ctd outliers : ", length(boxplot.stats(data_cont[,i])$out)))
  print("---")
}

