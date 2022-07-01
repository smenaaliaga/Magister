# REGRESIÓN LOGÍSTICA
# NOS UBICAMOS EN LA RUTA DEL ARCHIVO
setwd("C:/Users/Mario/Desktop/mario/marioguzman/proyectos/diplomado/Diplomado PUCV/material/Regresion logistica/fuentes")
call_center = read.csv("CallCenterData.csv",
                       header = TRUE,
                       sep=";")
# CARGA DE LIBRERIAS
library(dplyr)
library(caret)
library(tidyverse)

# VISUALIZAR LA TABLA
call_center %>% head
View(call_center)

# TRANSFORMAR TODAS LAS VARIABLES DEL TIPO "CHARACTER" A FACTOR
call_center %>% summary
mutate_if(call_center,
          is.character,
          as.factor) %>% summary

call_center <- mutate_if(call_center,
                         is.character,
                         as.factor)

# EL RESUMEN NOS MUESTRA LAS FRECUENCIAS DE LA VARIABLES DEL TIPO FACTOR
call_center %>% summary

# ESCALADO DE VARIABLES
# ESCALADO NORMAL
func_norm <- function(x){
  dev_x <- sd(x)
  mean_x <- mean(x)
  ((x - mean_x)/dev_x)
}
# ESCALADO MIN-MAX
func_min_max <- function(x){
  min_x <- min(x)
  max_x <- max(x)
  ((x - min_x)/ (max_x - min_x))
}

# APLICAMOS EL ESCALADO NORMAL
call_center <- mutate_if(call_center,is.numeric,func_norm)
mutate_if(call_center,is.numeric,scale) %>% head
# APLICAMOS EL ESCALADO ESTÁNDAR
mutate_if(call_center,is.numeric,func_min_max) %>% head

# ELIMINAR VARIABLES QUE NO CONSIDERAREMOS
call_center_std <- call_center %>% select(-c(ID,ProdPromAprobada,ProdMayorUbral))
# VALIDACIÓN DE LA ESTANDARIZACIÓN
(32 - min(call_center$Edad))/ (max(call_center$Edad) - min(call_center$Edad))
call_center_std %>% head
(32 - mean(call_center$Edad)) /sd(call_center$Edad)

# TRANSFORMAR LA VARIABLE RESPUESTA
y <- call_center %>% select(ProdMayorUbral) %>% 
                  mutate(y = as.factor(
                    case_when(ProdMayorUbral=='si'~1,
                              TRUE ~ 0)
                  ))
y%>% head

# SELECCIONAR VARIABLES DEL TIPO FACTOR
factores <- select_if(call_center_std,is.factor)
factores %>% head
# SELECCIONAR VARIABLES DEL TIPO NUMÉRICO
numericas <- select_if(call_center_std,is.numeric)

# GENERACIÓN DE VARIABLES DUMMY
# el argumento levelsOnly T permite retornar las categórias de cada variable como columnas
# el argumento levelsOnly F permite retornar VARIABLE.CATEGORIA
fit_dummy = dummyVars(~.,data = factores,levelsOnly = T)
fit_dummy = dummyVars(~Sexo+NivelEducacion+EstadoCivil,
                      data = factores,
                      levelsOnly = F)
# GENERAR EL PREDICT PARA LLEVAR LAS VARIABLES A DUMMY
factors_dummy <- predict(fit_dummy,factores) %>% data.frame
factors_dummy %>% str

# MEDIR CORRELACIÓN
numericas %>% cor(method="spearman")

# GENERAR EL DATAFRAME PROCESADO (NUMÉRICAS + FACTORES + TARGET)
df_preproc <- numericas %>%
              add_column(factors_dummy) %>%
             mutate(y = y$y)

# GENERACIÓN DE MUESTRA ALEATORIA PARA EL CONJUNTO DE ENTRENAMIENTO Y PRUEBA
idx <- sample(1:nrow(df_preproc),floor(nrow(df_preproc)*0.7))
train <- df_preproc[idx,]
test <- df_preproc[-idx,]

# DISTRIBUCIÓN DE LA VARIABLE RESPUESTA
train %>% group_by(y) %>% 
      summarise(n=n(),porcentaje = n/nrow(train))
test %>% group_by(y) %>% 
      summarise(n=n(),porcentaje = n/nrow(test))

# SELECCIÓN DE VARIABLES
# MODELO NULO
modelo_nulo = glm(y~1,family="binomial", data=train)
# MODELO COMPLETO
modelo_full = glm(y~.,family="binomial", data=train)

# FORWARD
step(modelo_nulo,scope=(list(lower = modelo_nulo, 
                             upper = modelo_full)),
     data=train,direction = "forward")
#glm(formula = y ~ HorasLogeadas + HorasHabladas + Edad + RegistrosTerminados, 
# family = "binomial", data = train)

# BACKWARD
step(modelo_full,scope=(list(lower = modelo_nulo, 
                             upper = modelo_full)),
     data=train,direction = "backward")
# glm(formula = y ~ Edad + HorasLogeadas + HorasHabladas + RegistrosTerminados, 
#    family = "binomial", data = train)

# STEPWISE
step(modelo_nulo,
     scope=(list(lower = modelo_nulo, 
                 upper = modelo_full)),
     data=train, direction = "both")
# glm(formula = y ~ HorasLogeadas + HorasHabladas + Edad + RegistrosTerminados, 
# family = "binomial", data = train)

# MODELO DE REGRESIÓN LOGÍSTICA PRIORIZADO CON LA SELECCIÓN DE VARIABLES
fit <- glm(formula = y ~ HorasLogeadas + 
                          HorasHabladas + 
                          Edad + 
                          RegistrosTerminados, 
           family = "binomial", data = train)

# RESUMEN DEL MODELO
fit %>% summary
# COEFICIENTES
fit$coefficients
# GRÁFICA DE LOS RESIDUOS
plot(fit$residuals)
# NÚMERO DE ITERACIONES
fit$iter
# CONVERGENCIA
fit$converged
# PESOS
fit$weights

# PREDICCIONES
predicciones <- predict(fit,
                        newdata = test %>% select(-y),
                        type="response")
predicciones

# AGREGAR LAS PREDICCIONES AL CONJUNTO DE TEST
test <- test %>% 
  mutate(predicciones = predicciones,
         class_pred = case_when(predicciones>0.45~1,TRUE~0))

# GENERAR LA MATRIZ DE CONFUSIÓN
conf<- caret::confusionMatrix(as.factor(test$class_pred),
                       as.factor(test$y))

# ERROR
32/sum(conf$table)
1-conf$overall[1]

# RESUMEN POR CLASE
conf$byClass

# SUPUESTO DE KOLMOGOROV-SMIRNOV
library(InformationValue)
ks_stat(actuals = fit$y,
        predictedScores = fitted(fit))

# HIPOTESIS DE DISTRIBUCIÓN
library(ResourceSelection)
hoslem.test(fit$y,fitted(fit),g=10)

# FUNCIÓN SIGMOIDE
sigmoide <- function(x){
  sigmoide = (exp(x)/(1+exp(x)))
  sigmoide
}
sigmoide(0)

# GENERACIÓN DE DATA PARA GRAFICAR LA DISTRIBUCIÓN SIGMOIDE
df <- data.frame(x = seq(from = -10,to = 10,by = 0.1))
df <- df %>% mutate(y = sigmoide(x))

# COMPARACIÓN DE LA DISTRIBUCIÓN SIGMOIDE CON LA DISTRIBUCIÓN DE LA REGRESIÓN LOGÍSTICA
par(mfrow=c(1,2))
plot(df$x,df$y)
plot(sort(fitted.values(fit)))

# MATRIZ DE CONFUSIÓN ENTREGANDO LAS PROBABILIDADES Y UN threshold
InformationValue::confusionMatrix(test$y,
                                  test$predicciones,
                                  threshold = 0.45)