# ÁRBOLES DE DECISIÓN
# NOS UBICAMOS EN LA RUTA DEL ARCHIVO
setwd("C:/Users/Mario/Desktop/mario/marioguzman/proyectos/diplomado/Diplomado PUCV/material/Regresion logistica/fuentes")
call_center = read.csv("CallCenterData.csv",
                       header = TRUE,
                       sep=";")
# CARGA DE LIBRERIAS
library(dplyr)
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(ROCR)
# VISUALIZAR LA TABLA
call_center %>% head

# TRANSFORMAR TODAS LAS VARIABLES DEL TIPO "CHARACTER" A FACTOR
call_center %>% summary
mutate_if(call_center,
          is.character,
          as.factor) %>% summary
call_center %>% str
# TRANSFORMAR DE CARÁCTER A FACTOR
call_center <- mutate_if(call_center,
                         is.character,
                         as.factor)

# RESUMEN DE LA BASE DE DATOS
call_center %>% summary

# SELECCIÓN DE VARIABLES PARA CLASIFICACIÓN
df_preproc_clas <- call_center%>% select(-c(ID,ProdPromAprobada))

# SELECCIÓN DE VARIABLES PARA REGRESIÓN
df_preproc_reg <- call_center%>% select(-c(ID,ProdMayorUbral))

# GENERACIÓN DE MUESTRA ALEATORIA PARA EL CONJUNTO DE ENTRENAMIENTO Y PRUEBA
idx <- sample(1:nrow(df_preproc_clas),floor(nrow(df_preproc_clas)*0.7))
train <- df_preproc_clas[idx,]
test <- df_preproc_clas[-idx,]

# DISTRIBUCIÓN DE LA VARIABLE RESPUESTA
train %>% group_by(ProdMayorUbral) %>% 
  summarise(n=n(),porcentaje = n/nrow(train))
test %>% group_by(ProdMayorUbral) %>% 
  summarise(n=n(),porcentaje = n/nrow(test))

# CONSTRUCCIÓN DEL ÁRBOL DE CLASIFICACIÓN
fit_class <- rpart(ProdMayorUbral~.,data=train,
                   parms = list(split="information"))

# PARÁMETRO DE REGULARIZACIÓN
fit_class$cptable
# GRÁFICA DEL ÁRBOL
rpart.plot(fit_class,cex=0.6)
fit_class
# PARAMETRO DE CONTROL
fit_class$control
# PARAMETROS
fit_class$parms
fit_class$functions
# IMPORTANCIA DE LOS PREDICTORES
fit_class$variable.importance

fit_class$method
train$ProdMayorUbral %>% unique
# PREDICCIONES DE CLASE
pred<-predict(fit_class,newdata = test,type = "class")

# MATRIZ DE CONFUSIÓN
conf<-caret::confusionMatrix(pred,test$ProdMayorUbral)
conf$overall
conf$byClass

# PREDICCIONES DE PROBABILIDAD
pred <- predict(fit_class,newdata = test,type = "prob")
pred %>% head

# CONSTRUCCIÓN CURVA ROC
pred_roc <- ROCR::prediction(pred[,2],test$ProdMayorUbral)
roc_perf <- ROCR::performance(pred_roc, measure = "tpr", x.measure = "fpr")

# GRÁFICAMOS LA CURVA ROC
plot(roc_perf,
     colorize = TRUE,
     text.adj = c(-0.2,1.7),
     print.cutoffs.at = seq(0,1,0.1))
abline(a=0,b=1,col="brown")

# OBTENEMOS EL ÁREA BAJO LA CURVA 
auc = performance(pred_roc, measure = "auc")
auc = auc@y.values[[1]]
auc

# CONSTRUCCIÓN DE ÁRBOL DE CLASIFICACIÓN CON CIERTOS PARÁMETROS.
fit_class <- rpart(ProdMayorUbral~.,data=train,
                   parms = list(split="information"),
                   control=list(cp=0.00001,
                                maxdepth = 40,
                                minsplit=5))

# PARÁMETRO DE REGULARIZACIÓN
fit_class$cptable

# GRÁFICA DEL ÁRBOL
rpart.plot(poda,cex=0.5)

# GRÁFICA DEL CP CON RESPECTO A SU ERROR
plot(fit_class$cptable[,"CP"],
     fit_class$cptable[,"xerror"],
     type="b",
     xlab="Coeficiente CP",
     ylab="log Error promedio")

# ENCONTRAR EL CP ÓPTIMO
cp_opt <- fit_class$cptable[which.min(fit_class$cptable[,"xerror"])
                                         ,"CP"]
cp_opt

# PODA DEL ÁRBOL
poda <- prune(fit_class,cp=cp_opt)
rpart.plot(poda)

# MODELO DE REGRESIÓN
df_preproc_reg <- call_center%>% 
                    select(-c(ID,ProdMayorUbral))

# GENERACIÓN DE MUESTRA ALEATORIA PARA EL CONJUNTO DE ENTRENAMIENTO Y PRUEBA
idx <- sample(1:nrow(df_preproc_reg),floor(nrow(df_preproc_reg)*0.7))
train <- df_preproc_reg[idx,]
test <- df_preproc_reg[-idx,]
train$ProdPromAprobada

# CONSTRUCCIÓN ÁRBOL DE REGRESIÓN
fit_reg <- rpart(ProdPromAprobada~.,data=train,
                 control=list(cp=0.00001))

# IMPORTANCIA DE LAS VARIABLES
fit_reg$variable.importance
fit_reg$cptable
rpart.plot(fit_reg)

# ENCONTRAR EL CP ÓPTIMO
cp_opt <- fit_reg$cptable[which.min(fit_reg$cptable[,"xerror"])
                            ,"CP"]
cp_opt
fit_reg$cptable

# PODA DEL ÁRBOL DE REGRESIÓN
poda_reg <- prune(fit_reg,cp=cp_opt)
rpart.plot(fit_reg)

# PREDICCIONES
pred_reg <- predict(fit_reg,newdata=test)

# PREDICCIONES DISTINTAS
pred_reg %>% unique

# CÁLCULO DEL MSE
MSE <- (1/nrow(test))*sum((test$ProdPromAprobada-pred_reg)^2)

# RESUMEN DE LA VARIABLE DEPENDIENTE
test$ProdPromAprobada %>% summary

# REVISIÓN DE LAS PREDICCIONES
test%>% mutate(pred_reg = pred_reg) %>% 
          select(ProdPromAprobada,pred_reg) %>% 
          head
