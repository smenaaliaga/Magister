library(MASS)
library(caret)
# GENERAR NÚMERO DE DATOS
n_1 = 100
n_2 = 100
# GENERACIÓN DE POBLACIÓN
p = 2
# GENERACIÓN DE VECTORES DE MEDIAS (POBLACIONAL)
mu_1 = matrix(c(1,0),nc=2)
mu_2 = matrix(c(0,1),nc=2)
# GENERACIÓN DE MATRIZ DE VARIANZAS Y COVARIANZAS (POBLACIONAL)
Sigma = matrix(c(1,0.3,0.3,1),nc=2)
# MUESTRA DE VALORES
mu_1;mu_2;Sigma

# GENERACIÓN DE VALORES NORMALES MULTIVARIADOS PARA POBLACIO 1 Y 2
x_1 = mvrnorm(n_1,mu_1,Sigma)
x_2 = mvrnorm(n_2,mu_2,Sigma)

# GENERACIÓN DE VECTOR DE MEDIAS (ESTIMADO)
x_bar_1 = apply(x_1,2,mean)
x_bar_2 = apply(x_2,2,mean)
# GENERACIÓN DE MATRIZ DE VARIANZAS Y COVARIANZAS (ESTIMADO)
S = ((n_1-1)*var(x_1)+(n_2-1)*var(x_2))/(n_1+n_2-2)
# x^t * b- a >=0
# ESTIMANDO EL VALOR DE LA PENDIENTE 
# SIGMA^{-1} * (MU_1 - MU_2)
b = solve(S)%*%(x_bar_1-x_bar_2)
# 0.5 * (MU_1 + MU_2)* SIGMA^{-1}* (MU_1-MU_2)
a = 0.5 * t(x_bar_1+x_bar_2)%*%solve(S)%*%(x_bar_1-x_bar_2)
# GENERAMOS LA ETIQUETA A CADA POBLACIÓN
x_1 = cbind(x_1,1)
x_2 = cbind(x_2,2)
# JUNTAMOS LOS DATOS EN UN SOLO VECTOR POR FILAS
data = rbind(x_1,x_2)
# GENERAMOS LA REGION CRITICA Y CONVERTIMOS A DATAFRAME
data_pred = data.frame((data[,-3] %*% b) - a[1])
# CAMBIAMOS EL NOMBRE DE LA COLUMNA POR "discriminante"
names(data_pred)<-"discriminante"
# GENERAMOS EL CRITERIO DE PERTENECER A UNA POBLACIÓN (x^t * b - a >= 0 ) 
# PERTENECE A LA POBLACIÓN 1
data_pred$pred <- ifelse(data_pred$discriminante>=0,1,2)
data_pred
# CONVERTIR A DATA FRAME
data = data.frame(data)
# GENERAR LA PREDICCION COMO COLUMNA EN EL DATA ORIGINAL
data$prediccion <- data_pred$pred
# GENERAR MATRIZ DE CONFUSIÓN
confusion <-table(data$X3,data$prediccion)
# ERROR DE CLASIFICACIÓN
1- sum(diag(confusion))/sum(confusion)
# GRÁFICA DE LOS PUNTOS DE X1
plot(x_1,pch="1",col=2,xlim=c(-5,5),ylim=c(-5,5))
# AGREGAMOS LOS PUNTOS DE X2
points(x_2,pch="2",col=5)
# AGREGAMOS UN NUEVO PUNTO
points(new_point,pch=19,col=3,cex=2)
# NUEVO PUNTO
new_point = matrix(c(0,0),1)

(new_point %*% b) - a[1]
# GENERACIÓN DE UNA NUEVA GRILLA
new_data <- expand.grid(seq(-4,4,by=0.1),seq(-4,4,by=0.1))
# PASAMOS A MATRIZ LA NUEVA DATA
new_data = as.matrix(new_data)
# GENERACIÓN DE PREDICCIONES NUEVA DATA
pred =  (new_data %*% b) - a[1]
new_data = data.frame(new_data)
# REGION DE CLASIFICACIÓN
new_data$etiqueta<- ifelse(pred>=0,1,2)
table(new_data$etiqueta)

etiqueta_1 = subset(new_data,etiqueta==1)[,1:2]
etiqueta_2 = subset(new_data,etiqueta==2)[,1:2]
# GRÁFICA DE LOS PUNTOS DE X1
plot(new_data[,1:2],pch="1",col=5,xlim=c(-5,5),ylim=c(-5,5))
points(etiqueta_1,pch=20,col=4,cex=1.2)
points(etiqueta_2,col=7,pch=20,cex=1.2)
# INDICE ALEATORIO DE UNA MUESTRA DEL 70% DEL CONJUNTO DE DATOS
index = sample(1:nrow(new_data),nrow(new_data)*0.7)
train = new_data[index,]
test = new_data[-index,]
names(train)
# MODELO DE LDA
lda_1 = lda(etiqueta ~ Var1+Var2, data=train)
pred_1 = predict(lda_1,newdata = test[,1:2])
# PROBABILIDAD A POSTERIOR
pred_1$posterior[1,]
# VALOR CRÍTICO
pred_1$x[1,]
# CLASE ESTIMADA
pred_1$class
# CLASE PREDICTIVA
test$prediccion<- pred_1$class
# TABLA DE CONFUSIÓN
table(test$etiqueta,test$prediccion)
# MATRIZ DE CONFUSIÓN
confusionMatrix(as.factor(test$prediccion),as.factor(test$etiqueta))

# MODELO QDA
qda_1 = qda(etiqueta~Var1+Var2,data=train)
# DISTRIBUCIÓN A PRIORI
qda_1$prior
# CANTIDAD DE DATOS POR ETIQUETA
qda_1$counts
# CANTIDAD DE DATOS TOTALES
qda_1$N
# PREDICCIONES QDA
pred_2  = predict(qda_1,newdata = test[,1:2])
# CLASE
pred_2$class[1]
# PROBABILIDAD A POSTERIOR
pred_2$posterior[1,]
# PREDICCIONES TEST
test$prediccion2<-pred_2$class
# MATRIZ DE CONFUSIÓN
table(test$etiqueta,test$prediccion2)
confusionMatrix(as.factor(test$prediccion2),as.factor(test$etiqueta))
