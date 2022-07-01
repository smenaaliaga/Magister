install.packages('glmnet')
install.packages('mlbench')

library(glmnet)
library(mlbench)
library(ggplot)
library(caret)
library(tidyverse)

data('PimaIndiansDiabetes2', package = 'mlbench')

PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

# pregnat : # veces en que se ha embarazado
# glucose : Test de tolerancia a glucosa
# pressure : Presion sanguinea Diastólica
# triceps : Grosor en pliegues de la piel
# insulin : Concentracion de insulina luego de 2 horas
# mass : indice de masa corporal
# pedigree : Funcion de 

x <- model.matrix(diabetes ~ ., PimaIndiansDiabetes2)
x
y <- ifelse(PimaIndiansDiabetes2$diabetes == 'pos', 1, 0)
y

### glmnet
# alpha = 1 : lasso
# alpha = 0 : ridge
# alpha ~ ]0, 1[ : elastic net

l1mod_1 <- glmnet(x, y, family = 'binomial', lambda = 1.0, alpha = 1)
coef(l1mod_1)

l1mod_2 <- glmnet(x, y, family = 'binomial', lambda = 0.1, alpha = 1)
coef(l1mod_2)

l1mod_3 <- glmnet(x, y, family = 'binomial', lambda = 0.01, alpha = 1)
coef(l1mod_3)

l1mod_4 <- glmnet(x, y, family = 'binomial', lambda = 0.001, alpha = 1)
coef(l1mod_4)

# Modelo de Regresion Logistica Original, lambda = 0
lr1 <- glm(diabetes ~ ., data = PimaIndiansDiabetes2, family = binomial(link = logit))
summary(lr1)

# CARET 
# Test y Training
set.seed(111)
training_samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = .8, list = F)
training_samples

train.data <- PimaIndiansDiabetes2[training_samples, ]
test.data <- PimaIndiansDiabetes2[-training_samples, ]

x_train <- model.matrix(diabetes ~ ., train.data)[, -1]
y_train <- ifelse(train.data$diabetes == 'pos', 1, 0)

# Busqueda de lambdas optimos con Cross-Validation
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, type.measure = 'class', n_folds = 10)
plot(cv.lasso)


# OTRA FORMA DE ENTRENAR EN CARET

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmFit1 <- train(diabetes ~ ., data = train.data, 
                 method = "glmnet", 
                 trControl = fitControl,
                 verbose = FALSE)

gbmFit1

plot(gbmFit1)  

predict(gbmFit1)
