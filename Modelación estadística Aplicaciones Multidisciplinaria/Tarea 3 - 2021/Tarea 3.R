setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 3 - 2021")

# install.packages("tidyverse")
# install.packages("likert")
# install.packages("corrplot")
# install.packages("Hmisc")
# install.packages("xtable")
# install.packages("psych")

library(tidyverse)
library(likert)
library(corrplot)
library(Hmisc)
library(xtable)
library(psych)


data <- read.csv2("datosc1.csv")

################
## Pregunta 1 ##
################

# Variables de tipo numerico a tipo factor
data_factor <- data %>%
  mutate_if(sapply(data, is.numeric), as.factor)

# Variables a tipo likert
data_likert <- likert(data_factor)

# Resumen
summary(data)
summary(data_factor)
summary(data_likert)
?xtable
xtable(data_likert)

# Plots
likert.bar.plot(data_likert) + theme_minimal() + theme(legend.position = 'bottom')
likert.density.plot(data_likert)
likert.heat.plot(data_likert)
likert.histogram.plot(data_likert)

################
## Pregunta 2 ##
################

# INVERTIR 

?reverse.levels
data_reves <- reverse.levels(data_factor[,c(  'Y2', 'Y3', 'Y5', 'Y6', 'Y9', 
                                              'Y10', 'Y11', 'Y12', 'Y14', 'Y16', 
                                              'Y18','Y19', 'Y20', 'Y21', 'Y22',
                                              'Y25', 'Y26', 'Y27', 'Y28')])
data_final <- data_factor[,c('Y1', 'Y4', 'Y7', 'Y8', 'Y13', 'Y15', 'Y17',
                             'Y23', 'Y24', 'Y29')]
data_final <- cbind(data_final, data_reves)

likert_final <- likert(data_final)
summary(likert_final)
xtable(likert_final)
likert.bar.plot(likert_final) + theme_minimal() + theme(legend.position = 'bottom')

data_final_numeric <- data_final %>%
  mutate_if(sapply(data_final, is.factor), as.numeric)

### CORRELACIONES TOTALES

likert_final$items$Y2
as.numeric(likert_final$items$Y2)

# convertir items invertidos en numericos
data_final_numeric <- likert_final$items %>%
  mutate_if(sapply(likert_final$items, is.factor), as.numeric)

cor_antes <- cor(data, method = 'spearman')
cor_dsps <- cor(data_final_numeric, method = 'spearman')

par(mfrow=c(1,2))
corrplot(cor_antes, method = 'color', order = 'alphabet')
corrplot(cor_dsps, method = 'color', order = 'alphabet')

  ### Cognoscitivo

par(mfrow=c(1,1))

data_cogn <- data_final_numeric[,c('Y3', 'Y4', 'Y6', 'Y17', 'Y18', 'Y22', 'Y24', 'Y26', 'Y27', 'Y28')]
cor_cogn <- cor(data_cogn, method = 'spearman')
corrplot(cor_cogn, method = 'number', order = 'alphabet')
cortest_cogn <- rcorr(cor_cogn, type = 'spearman')

?xtable
print(xtable(cortest_cogn[["P"]]))

  ### Emocional

data_emoc <- data_final_numeric[,c('Y1', 'Y2', 'Y11', 'Y14', 'Y15', 'Y21', 'Y23', 'Y25')]
cor_emoc <- cor(data_emoc, method = 'spearman')
corrplot(cor_emoc, method = 'number', order = 'alphabet')
cortest_emoc <- rcorr(cor_emoc, type = 'spearman')

print(xtable(cortest_emoc[["P"]]))

  ### Tendencia a la accion

data_tend <- data_final_numeric[,c('Y5', 'Y7', 'Y8', 'Y9', 'Y10', 'Y12', 'Y13', 'Y16', 'Y19', 'Y20', 'Y29')]
cor_tend <- cor(data_tend, method = 'spearman')
corrplot(cor_tend, method = 'number', order = 'alphabet')
cortest_tend <- rcorr(cor_tend, type = 'spearman')

print(xtable(cortest_tend[["P"]]))



################
## Pregunta 3 ##
################

?psych::alpha
alfa <- psych::alpha(data_final_numeric)
alfa

# Cognoscitivo
psych::alpha(data_cogn)

# Emocional
psych::alpha(data_emoc)

# Tendencia a la accion
psych::alpha(data_tend)
