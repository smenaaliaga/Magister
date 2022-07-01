# install.packages('dplyr')
# install.packages("tidymodels")
library(dplyr)
library(nnet)
library(knitr)
library(tidymodels)
# Set Working Directory in Desktop
setwd('~/Desktop')
# Read nhanes_adult.csv
df <- read.csv('nhanes_adult.csv')
# View head df
head(df)
# ¿Podremos usar la edad y su actividad física para predecir su estado de salud?
# AED
glimpse(df)
# Conversion a factor
df$HealthGen <- as.factor(df$HealthGen)
df$PhysActive <- as.factor(df$PhysActive)
# Vistazo
glimpse(df)
# Resumen númerica
summary(df)
# Ajustar a Regresión Logística Multinomial
fit <- nnet::multinom(HealthGen ~ Age + PhysActive, df)
fit
# Formato de ajuste resultado
tidy(fit, exponentiate = F, conf.int = T) %>% kable(digits = 3, format = 'simple')
# Predicciones
pred <- predict(fit, type = 'probs')
# Formato de prediccion
as_tibble(pred) %>% mutate(obs_num = 1:n()) %>% slice(1:10)
# Prediccion de un dato nuevo
newdata = list('Age' = 34, 'PhysActive' = factor('No'))
predict(fit, type = 'probs', newdata = newdata)
  