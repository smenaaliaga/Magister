
install.packages('VGAM')
library(tidyverse)
library(VGAM)
# set working directory
setwd('~/Documents/Magister/Modelos Lineales para Clasificación/LR Ordinal')

# DATOS DE POLITICA

# read data
polviews <- read.table('Polviews.csv', sep = ';', header = 1)
# vista de datos 
View(polviews)
# ajuste
fit_p <- vglm(cbind(y1,y2,y3,y4,y5) ~ party + gender, 
            data = polviews,
            family=cumulative(parallel = T))
# summary
summary(fit_p)
# RESULTADOS
# (Intercept): n -> tau que separa 
# luego siguen los betas
# en este caso el genero parece no ser significativo para el modelo



# DATOS DE INVESTIGACION EN CELULAR MADRES

# Opiniones en 4 niveles respecto  al apoyo a investigación en celulas madres
# La única covariable es la creencia religiosa de quien responde
# Queremos investigar en que medida las creencias religiosas permiten
# predecir inclinacion acerca de financiar investigacion en celulas madres

# Var. respuesta : inclinación a financiar investigación
# 1 : no financiar
# 2 : probablemente no financiar
# 3 : probablemente financiar
# 4 : definitivamente financiar

# read data
survey <- read.table('stem cell research survey.csv', sep = ';', header = 1)
# vista de datos 
View(survey)
# ajuste
fit_s <- vglm(cbind(y1,y2,y3,y4) ~ creencia, 
            data = survey,
            family=cumulative(parallel = T))
# summary
summary(fit_s)

# Como leer Exponentiated


# Como fijar el nivel de referencias para VGLM
survey$creencia <- as.factor(survey$creencia)
survey$creencia <- relevel(survey$creencia, ref='liberal')




# RODILLA 

install.packages('catdata')

library(catdata)

data(knee)

knee

# Th. 1: placebo ; 2: tratamiento
# R4. Como se siente luego de 10 dias de Th

chisq.test(knee$Th, knee$R4)
# Hay siginificancia entre las diferencias, pero....
# DComo cuantificamos esas diferencias???

knee$Th <- as.factor(knee$Th)
knee$R4 <- as.ordered(knee$R4)

# ajuste
fit_k <- vglm(cbind(R4) ~ Th + Age, 
              data = knee,
              family=cumulative(parallel = T))
# summary
summary(fit_k)

# usando Libreria MASS
library(MASS)

plr1 <- polr(R4 ~ Th + Age, method = "logistic", data = knee)
summary(plr1)
exp(-coef(plr1))
