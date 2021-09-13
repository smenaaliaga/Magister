setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 3 - 2021")

# install.packages("tidyverse")
# install.packages("likert")

library(tidyverse)
library(likert)

data <- read.csv2("datosc1.csv")

################
## Pregunta 1 ##
################

data_factor <- data %>%
  mutate_if(sapply(data, is.numeric), as.factor)

likert <- likert(data_factor)

summary(data)
summary(data_factor)
summary(likert)
?xtable
xtable(likert)

likert.bar.plot(likert) + theme_minimal() + theme(legend.position = 'bottom')
likert.density.plot(likert)
likert.heat.plot(likert)
likert.histogram.plot(likert)



data_reves <- reverse.levels(data)
likert_reves <- likert(data_reves)
summary(likert_reves)