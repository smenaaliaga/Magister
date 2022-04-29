# setear ubicación del proyecto
setwd('Documents/Magister/Metodos Multivariados, Clasificacion y Pronostico/Tarea 1')
# Librerias
# install.packages("tidyverse")
# install.packages('xtable')
# install.packages("GGally")
# install.packages('MVN')
# install.packages('car')
# install.packages("biotools")
# install.packages('caret')
# install.packages('performanceEstimation')
# install.packages('ROCit')
# install.packages('ResourceSelection')
library(tidyverse)
library(xtable)
library(GGally)
library(MVN)
library(car)
library(biotools)
library(MASS)
library(caret)
library(performanceEstimation)
library(ROCit)
library(ResourceSelection)
  # Exportacion de datos
data = read.csv("churn-analysis.csv", header = TRUE, sep=";")
# tipos de datos en df
str(data)
# Transformacion de datos character a factor
data <- data %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(area.code = as.factor(area.code)) %>% 
  select(-c('phone.number'))
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
  select_if(~!is.factor(.)) %>%
  add_column(churn = data$churn)
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
count_outliers_df <- function(df) {
  for(i in 1:ncol(df)){
    print(colnames(df)[i])
    print(boxplot.stats(df[,i])$out)
    print(paste0("Ctd outliers : ", length(boxplot.stats(df[,i])$out)))
    print("---")
  }
}
count_outliers_df(data_cont)
## LDA ##
#########
## Test MVN de normalidad multivariante
?mvn
test_mvn <- function(df) {
  df_schurn <- df[,-15]
  data_split <- split(df_schurn, df$churn)
  mvnTests = c('mardia', 'hz')
  for(test in mvnTests){
    print(paste0("test MVN : ", test))
    for(i in 1:length(data_split)){
      print(paste0("churn : ", names(data_split)[i]))
      print(mvn(data_split[[i]], mvnTest = test, scale = T))
    }
  }
}
test_mvn(data_cont)
# removiendo todos los outleris de un dataframe
remove_outliers_df <- function(df) {
  df_new <- df
  for(i in 1:(ncol(df)-1)){
    outliers <- boxplot(df_new[,i], plot = FALSE)$out
    df_new <- df_new[!(df_new[,i] %in% outliers), ]
  }
  return(df_new)
}
data_sout <- remove_outliers_df(data_cont)
count_outliers_df(data_sout)
# test sin outliers
test_mvn(data_sout)
mvn(data_cont[,-15], mvnTest = 'mardia', scale = T)
a <- mvn(data_cont[,-15], mvnTest = 'mardia', scale = T)$univariateNormality
xtable(mvn(data_cont[,-15], mvnTest = 'mardia', scale = T)$univariateNormality)

mvn(data_cont[,-15], mvnTest = 'hz', scale = T)$multivariateNormality
mvn(data_sout[,-15], mvnTest = 'hz', scale = T)$multivariateNormality
## Homecedasticidad de la varianza
?boxM
boxM(data = data_cont[, -15], grouping = data_cont[, 15])

others_b <- data_cont %>% 
  dplyr::select(-c('number.vmail.messages',
                   'customer.service.calls',
                   'total.intl.charge',
                   'total.intl.minutes',
                   'total.day.minutes',
                   'total.day.charge'))
mvn(others_b[, -ncol(others_b)], mvnTest = 'mardia', scale = T)$multivariateNormality
boxM(data = others_b[, -ncol(others_b)], grouping = others_b[, ncol(others_b)])
## Test y entrenamiento
# Estimate preprocessing parameters
preproc.parameter <- train %>% 
  preProcess(method = c("center", "scale"))
index = sample(1:nrow(others_b),nrow(others_b)*0.7)
train = others_b[index,]
test = others_b[-index,]
# Transform the data using the estimated parameters
train.transform <- preproc.parameter %>% predict(train)
test.transform <- preproc.parameter %>% predict(test)
## Estimacion de parametros
modelo_lda <- lda(formula = churn ~ .,data = train.transform)
## Predicciones
pred_1 = predict(modelo_lda, newdata = test.transform[-15])
# clase predictiva
test.transform$prediccion <- pred_1$class
# Matriz de confusión
lda_cm <- confusionMatrix(test.transform$prediccion,
                          test.transform$churn,
                          mode = "everything")
lda_cm
xtable(as.table(
  lda_cm
), type = "latex")

pred <- prediction(pred_1$posterior[,2], test.transform$churn) 
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# Balanceo de la data
datos_bal_over <- ovun.sample(churn ~ ., 
                              data = others_b, 
                              method = "over", 
                              N = table(others_b$churn)[1]*2)$data
index_2 = sample(1:nrow(datos_bal_over),nrow(datos_bal_over)*0.7)
train_2 = datos_bal_over[index_2,]
test_2 = datos_bal_over[-index_2,]
# Transform the data using the estimated parameters
train.transform_2 <- preproc.parameter %>% predict(train_2)
test.transform_2 <- preproc.parameter %>% predict(test_2)
## Estimacion de parametros
modelo_lda_2 <- lda(formula = churn ~ .,data = train.transform_2)
## Predicciones
pred_2 <-  predict(modelo_lda_2, newdata = test.transform_2[-9])
# clase predictiva
test.transform_2$prediccion <- pred_2$class
# Matriz de confusión
lda_cm_2 <- confusionMatrix(test.transform_2$prediccion,
                          test.transform_2$churn,
                          mode = "everything")
lda_cm_2
xtable(as.table(
  lda_cm_2
), type = "latex")
# Curva ROC
pred <- prediction(pred_2$posterior[,2], test.transform_2$churn) 
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

## Regresion Logistica ##
#########################
# Estandarizacion de variables cuantitativas
standard_norm = function(x){
  z = (x - mean(x))/sd(x)
}
data_std <- data %>% 
  select_if(is.numeric) %>%
  mutate_if(is.numeric, standard_norm)
# Variables dummmy de categoricas
data_factor <- data %>% 
  select_if(is.factor)%>%
  select(-churn)
model_dummy <- dummyVars(~.,data=data_factor)
data_dummy <- predict(model_dummy,data_factor)
data_dummy <- data_dummy%>% data.frame()
# Variable de respuesta Y
target <- data %>% 
  mutate(churn = case_when(churn=="yes"~1,TRUE~0)) %>%
  select(churn)
# Append estandarizacion y dummys
data_fin <- data_std %>% 
  add_column(data_dummy) %>%
  add_column(data$churn)
data_fin <- dplyr::rename(data_fin, churn = c('data$churn'))
names(data_fin)
# Correlacion de variables numericas
data_std %>% cor(method="kendall")
data_std %>% cor(method="pearson")
data_std %>% cor(method="spearman")
# Entrenamiento y testing
index = sample(dim(data_fin)[1],floor(dim(data_fin)[1])*0.7)
train = data_fin[index,]
test = data_fin[-index,]
table(train$churn)
table(test$churn)
# BALANCE DE LA VARIABLE RESPUESTA
train %>% group_by(churn) %>% summarise(conteo = n())
test %>% group_by(churn) %>% summarise(conteo = n())
table(train$churn)
table(test$churn)
## Seleccion de modelo
modelo_nulo = glm(churn~1,family = "binomial", data=train)
modelo_completo = glm(churn~.,family = "binomial", data=train)
# FORWARD
step(modelo_nulo,scope=(list(lower = modelo_nulo, upper = modelo_completo)),data=train,direction = "forward")
# BACKWARD
step(modelo_completo,data=train,direction = "backward")
# STEPWISE
step(modelo_nulo,scope=(list(lower = modelo_nulo, upper = modelo_completo)),data=train, direction = "both")
# Modelo de regresion logistica optimo conmetodo FORWARD
modelo_rl <- glm(churn ~ total.day.charge + international.plan.no + customer.service.calls + total.eve.minutes + voice.mail.plan.no + total.intl.charge + total.intl.calls + total.night.charge + state.MT + state.VA + state.TX + state.CA + state.WA + state.MI + state.NJ + state.SC + state.MS + state.RI + state.HI + state.MA,
                data = train, family = "binomial")
# coeficientes del modelo (betas)
xtable(as.table(
  modelo_rl$coefficients
), type = "latex")
modelo_rl$coefficients
#  Validacion del modelo
library(InformationValue)
ks_stat(actuals = modelo_rl$y,predictedScores = fitted(modelo_rl))
# Matriz de confusión
predicciones <- predict(modelo_rl,
                        newdata = test%>%select(-c(churn)), 
                        type="response")
predicciones
test$predicciones <- predicciones
test$predicciones <- ifelse(test$predicciones>0.6,'True','False')
rl_mc <- caret::confusionMatrix(as.factor(test$predicciones),
                                as.factor(test$churn),
                                mode = "everything")
rl_mc
xtable(as.table(
  rl_mc
), type = "latex")
# Curva ROC
library(ROCR)
pred = predict(modelo_rl,newdata = test%>%select(-c(churn)), type="response")
roc_pred = prediction(pred, test$churn)
roc_perf = performance(roc_pred, measure = "tpr", x.measure = "fpr")
# GCurva ROC
plot(roc_perf,
     colorize = TRUE,
     text.adj = c(-0.2,1.7),
     print.cutoffs.at = seq(0,1,0.1))
abline(a=0,b=1,col="brown")

