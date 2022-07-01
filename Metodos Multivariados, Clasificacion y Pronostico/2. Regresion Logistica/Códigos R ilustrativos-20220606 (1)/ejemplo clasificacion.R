# REGRESIÓN LOGÍSTICA
# NOS UBICAMOS EN LA RUTA DEL ARCHIVO
setwd("C:/Users/Mario/Desktop/mario/marioguzman/proyectos/diplomado/Diplomado PUCV/material/Regresion logistica/fuentes")
call_center = read.csv("CallCenterData.csv",header = TRUE,sep=";")

library(tidyverse)
library(caret)
call_center %>% head

# ANALISIS DESCRIPTIVO
# CONJUNTO DE VARIABLES QUE CUMPLE EL CRITERIO DE SER
# CARACTER LO PASA A TIPO FACTOR
call_center = mutate_if(call_center,
                        is.character,
                        as.factor)
call_center %>% summary
# GENERACIÓN DE FUNCIÓN DE ESTANDARIZACION NORMAL
standard_norm = function(x){
  z = (x - mean(x))/sd(x)
}
# 
call_center_std = mutate_if(call_center,
                            is.numeric,
                            standard_norm) %>%
  select(-c(ID,ProdPromAprobada))
# TAMBIEN PODEMOS REVISAR CON LA FUNCIÓN SCALE
# call_center_std = mutate_if(call_center,
#                            is.numeric,
#                            scale) %>%
#                     select(-c(ID,ProdPromAprobada))

# GENERACIÓN DE VARIABLES DUMMYS
# FILTRAMOS SOLO LAS VARIABLES DEL TIPO FACTOR
call_center_factor = select_if(call_center_std,is.factor)%>%
                      select(-ProdMayorUbral)
# GENERACIÓN DE MODELO PARA GENERAR ESTANDARIZACIÓN 
# DE VARIABLES DEL TIPO FACTOR
model_std_factor = dummyVars(~.,data=call_center_factor,levelsOnly =T)
# GENERAMOS EL PREDICT PARA LA CONSTRUCCIÓN DE VARIABLES
# DUMMYS
call_center_factor_std = predict(model_std_factor,call_center_std)
call_center_factor_std<-call_center_factor_std%>% data.frame()
call_center_factor_std %>% names
# VARIABLE RESPUESTA
target <- call_center_std %>% mutate(y = case_when(ProdMayorUbral=="si"~1,
                                                   TRUE~0))%>%
          select(y)
# FILTRAR VARIABLES NUMERICAS ESTANDARIZADAS
numeric_features = select_if(call_center_std,is.numeric)

# JUNTAR COLUMNAS NUMERICAS, DUMMYS Y TARGET
df <- numeric_features %>% 
      add_column(call_center_factor_std)%>%
      add_column(target)
df  

# CORRELACIONES DE VARIABLES NUMÉRICAS
numeric_features %>% cor(method="kendall")
numeric_features %>% cor(method="pearson")
numeric_features %>% cor(method="spearman")

# SEPARAR CONJUNTO DE ENTRENAMIENTO Y PRUEBA
index = sample(dim(df)[1],floor(dim(df)[1])*0.7)
train = df[index,]
test = df[-index,]
# BALANCE DE LA VARIABLE RESPUESTA
train %>% group_by(y) %>% summarise(conteo = n())
test %>% group_by(y) %>% summarise(conteo = n())
table(train$y)
table(test$y)

# SELECCIÓN DE VARIABLE
# MODELO NULO
modelo_nulo = glm(y~1,family = "binomial",data=train)
modelo_completo = glm(y~.,family = "binomial",data=train)
# 
#modelo_completo = glm(y~Edad + HorasLogeadas +
#                        HorasHabladas +
#                        ContactosEfectivosPromedio +
#                        RegistrosTerminados +
#                        m +f + em + sec +
#                        fue 
#                      ,family = "binomial",data=train)

# FORWARD
step(modelo_nulo,scope=(list(lower = modelo_nulo, upper = modelo_completo)),data=train,direction = "forward")
# BACKWARD
step(modelo_completo,data=train,direction = "backward")
# STEPWISE
step(modelo_nulo,scope=(list(lower = modelo_nulo, upper = modelo_completo)),data=train, direction = "both")
# MODELO DE REGRESIÓN LOGÍSTICA OPTIMO CON RESPECTO AL MÉTODO BACKWARD
modelo_rl = glm(y ~ Edad + HorasLogeadas + HorasHabladas + RegistrosTerminados + c,
                data = train,
                family = "binomial")
# coeficientes del modelo (betas)
modelo_rl$coefficients
# 
modelo_rl$residuals
# probailidades de cada observación
modelo_rl$fitted.values
# VALIDAR MODELO TEST HOSMER LEMESHOW
library(ResourceSelection)
hoslem.test(modelo_rl$y,fitted(modelo_rl))
# MÉTODO DEL K-S
library(InformationValue)
ks_stat(actuals = modelo_rl$y,predictedScores = fitted(modelo_rl))
# CALCULO DE PREDICCIONES PARA EL CONJUNTO DE TEST
predicciones = predict(modelo_rl,newdata = test%>%select(-c(y)), type="response")
predicciones
# GENERAMOS EL CORTE EN 0.5 PARA EL CORTE DE CLASE
test$predicciones <- predicciones
test$predicciones <- ifelse(test$predicciones>0.6,1,0)
table(test$y,test$predicciones)
# GENERAMOS LA MENCIÓN DE LA LIBRERIA PARA UTILIZAR LA FUNCIÓN confusionMatrix,
# DEBIDO A QUE LA LIBRERIA InformationValue POSEE UNA FUNCIÓN CON EL MISMO NOMBRE 
caret::confusionMatrix(as.factor(test$predicciones),as.factor(test$y))

# OBTENEMOS LAS CURVAS ROC
library(ROCR)
pred = predict(modelo_rl,newdata = test%>%select(-c(y)), type="response")
roc_pred = prediction(pred, test$y)
roc_perf = performance(roc_pred, measure = "tpr", x.measure = "fpr")

# GRÁFICAMOS LA CURVA ROC
plot(roc_perf,
     colorize = TRUE,
     text.adj = c(-0.2,1.7),
     print.cutoffs.at = seq(0,1,0.1))
abline(a=0,b=1,col="brown")

# OBTENEMOS EL ÁREA BAJO LA CURVA 
auc = performance(roc_pred, measure = "auc")
auc = auc@y.values[[1]]
auc

saveRDS(model, "model_rl.rds")
super_model <- readRDS("model_rl.rds")
