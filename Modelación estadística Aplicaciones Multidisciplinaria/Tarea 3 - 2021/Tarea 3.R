setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 3 - 2021")

# install.packages("tidyverse")
# install.packages("likert")
# install.packages("corrplot")
# install.packages("Hmisc")
# install.packages("xtable")
# install.packages("psych")
# install.packages("reshape")

library(tidyverse)
library(likert)
library(corrplot)
library(Hmisc)
library(xtable)
library(psych)
library(reshape)


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

# Ordenar
data_final <- data_final %>% select('Y1', 'Y2', 'Y3','Y4', 'Y5', 'Y6', 'Y7', 
                                    'Y8', 'Y9', 'Y10', 'Y11', 'Y12', 'Y13',
                                    'Y14', 'Y15', 'Y16', 'Y17', 'Y18','Y19', 
                                    'Y20', 'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 
                                    'Y26', 'Y27', 'Y28', 'Y29')

likert_final <- likert(data_final)
summary(likert_final)
xtable(likert_final)
likert.bar.plot(likert_final) + theme_minimal() + theme(legend.position = 'bottom')

# Cambiar de factor a numerico (con items invertidos)
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

### Exclusión de preguntas
ncol(data_final_numeric)
names(data_final_numeric[1])

# rm(tmp_alfa)

for (i in 1:ncol(data_final_numeric)) {
  tmp <- data_final_numeric
  tmp[,i] <- NULL
  tmp
  if(i==1){
    tmp_alfa <- psych::alpha(tmp)$total[,c('raw_alpha','average_r')]
  }else{
    tmp_alfa <- rbind(tmp_alfa, psych::alpha(tmp)$total[,c('raw_alpha','average_r')], make.row.names = F)
  }
}

tmp_alfa
print(xtable(tmp_alfa, digits = 3))


################
## Pregunta 4 ##
################

install.packages("multicon")
library(multicon)

?splithalf.r
splithalf.r(data_final_numeric, sims = 1000, graph = TRUE, seed = 2)


#Split data (cases) into two equally and randomly.


function(X = data_final_numeric, seed = 2){
  # optional fixed seed
  if (!is.null(seed)) {set.seed(seed)} 
  
  X <- as.matrix(X)
  
  # if k = 2x, then lengths Y1 = Y2
  # if k = 2x+1, then lenths Y1 = Y2+1
  k <- nrow(data_final_numeric)
  index <- sample(1:k, ceiling(k/2))
  Y1 <- data_final_numeric[index, ]
  Y2 <- data_final_numeric[-index, ]
  # return(list(Y1, Y2))
}

#Split data (variables-item) into two equally and randomly.

SPLIT.ITEMS <- 
  function(X, seed = NULL)
  {
    # optional fixed seed
    if (!is.null(seed)) {set.seed(seed)} 
    
    X <- as.matrix(X)
    
    # if n = 2x, then lengths Y1 = Y2
    # if n = 2x+1, then lenths Y1 = Y2+1
    n <- ncol(X)
    index <- sample(1:n, ceiling(n/2))
    Y1 <- X[, index ]
    Y2 <- X[, -index]
    return(list(Y1, Y2)) 
  }

dump("SPLIT.ITEMS", file = "SPLIT.ITEMS.R")

#with psych package

?splitHalf
ah <- splitHalf(data_final_numeric, raw = T)
a

################
## Pregunta 5 ##
################

data_cogn
data_emoc
data_tend

### Resumen Cognoscitivo 
summary(data_cogn)
describe(data_cogn)
describe.by(data_cogn)
xtable(summary(data_cogn))

data_cogn_factor <- data_cogn %>%
  mutate_if(sapply(data_cogn, is.numeric), as.factor)
data_cogn_likert <- likert(data_cogn_factor) 

# Plots
likert.bar.plot(data_cogn_likert) + theme_minimal() + theme(legend.position = 'bottom')

data_cogn_melt <- melt(data_cogn)

df2 <- data_cogn_melt %>%
  count(variable, value) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n))

ggplot(df2, aes(x=reorder(value,prop), y=prop, fill = variable)) + 
  scale_fill_viridis_d() +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + 
  labs(y = "Proporcion", x="Escala") 


### Resumen Emocional 
summary(data_emoc)
xtable(summary(data_emoc))

data_emoc_factor <- data_emoc %>%
  mutate_if(sapply(data_emoc, is.numeric), as.factor)
data_emoc_likert <- likert(data_emoc_factor) 

# Plots
likert.bar.plot(data_emoc_likert) + theme_minimal() + theme(legend.position = 'bottom')

data_emoc_melt <- melt(data_emoc)

df2 <- data_emoc_melt %>%
  count(variable, value) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n))

ggplot(df2, aes(x=reorder(value,prop), y=prop, fill = variable)) + 
  scale_fill_viridis_d() +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + 
  labs(y = "Proporcion", x="Escala") 


### Resumen Tendencia a la Accion 
summary(data_tend)
xtable(summary(data_tend))

data_tend_factor <- data_tend %>%
  mutate_if(sapply(data_tend, is.numeric), as.factor)
data_tend_likert <- likert(data_tend_factor) 

# Plots
likert.bar.plot(data_tend_likert) + theme_minimal() + theme(legend.position = 'bottom')

data_tend_melt <- melt(data_tend)

df2 <- data_tend_melt %>%
  count(variable, value) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n))

ggplot(df2, aes(x=reorder(value,prop), y=prop, fill = variable)) + 
  scale_fill_viridis_d() +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() + 
  labs(y = "Proporcion", x="Escala") 



################
## Pregunta 6 ##
################

y1y7 <- data_final_numeric %>% select("Y1","Y7")
y1y7

cor(y1y7$Y1, y1y7$Y7, method="spearman")

t <- table(y1y7)
t = as.data.frame(t)

xtable(table(y1y7))

ggplot(y1y7) + geom_bin2d()

ggplot(t, aes(Y1, Y7, fill = Freq)) + geom_bin2d() + theme_minimal() 
