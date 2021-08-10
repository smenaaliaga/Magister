setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 1")

#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("corrplot")

library(tidyverse)
library(reshape2)
library(corrplot)

#############################
## TRANSFORMACION DE DATOS ##
#############################

original <- read.csv2("Base_T1_Modif.csv")

data <- original %>%
     select(
         id = Id_Atenci.n,
         fecha = Fecha,
         turno = Turno,
         delta_1 = deltaMinuto1,
         respon_2 = Responsable,
         tipo_carga = Tipo_carga,
         delta_2 = deltafecha2,
         delta_3 = SUMA_ATENCION_O1_delta_3,
         respon_3 = Responsable_O1,
         delta_4 = Min_Atenci.n_O2_delta_4,
         respon_4 = Responsable_O2,
         delta_5 = Min_port_salida_delta_5
         ) 

# SE CONVIERTEN LOS deltas EN NUMERICOS
data <- data %>% mutate(
       delta_1 = as.numeric(delta_1),
       delta_2 = as.numeric(delta_2),
       delta_3 = as.numeric(delta_3),
       delta_4 = as.numeric(delta_4),
       delta_5 = as.numeric(delta_5),
       tipo_carga = as.factor(tipo_carga)
     )

deltas <- data %>% select(delta_1, delta_2, delta_3, delta_4, delta_5)

# ?melt
deltas_fact <- melt(deltas)

####################################
## ANALISIS EXPLORATORIO DE DATOS ##
####################################
# ?ggplot

#### BOXPLOT DELTAS
ggplot(data = deltas_fact, 
      mapping = aes(variable, value, colour = variable)) +
      geom_boxplot(outlier.shape=NA, size = 1) +
      theme_minimal() +
      theme(legend.position = "none") +
      xlab("Deltas") + ylab("Minutos") +
      ylim(0,84)

summary(deltas)

boxplot(deltas, ylim=c(0,65))

#### DELTA 1
# resumen
summary(deltas$delta_1)
quantile(deltas$delta_1)
sd(deltas$delta_1)
sd(deltas$delta_1)/mean(deltas$delta_1)

# outliers
boxplot(deltas$delta_1, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(deltas$delta_1)))
ggplot(deltas, aes(delta_1)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  xlim(0,7.5)

# test normalidad
ks.test(x = deltas$delta_1,"pnorm", mean(deltas$delta_1), sd(deltas$delta_1))


#### DELTA 2
# resumen
summary(deltas$delta_2)
quantile(deltas$delta_2)
sd(deltas$delta_2)
sd(deltas$delta_2)/mean(deltas$delta_2)

# outliers
boxplot(deltas$delta_2, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(deltas$delta_2)))
ggplot(deltas, aes(delta_2)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  xlim(0,5)

#test normalidad
ks.test(x = deltas$delta_2,"pnorm", mean(deltas$delta_2), sd(deltas$delta_2))


#### DELTA 3
# resumen
summary(deltas$delta_3)
quantile(deltas$delta_3)
sd(deltas$delta_3)
sd(deltas$delta_3)/mean(deltas$delta_3)

# outliers
boxplot(deltas$delta_3, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(deltas$delta_3)))
ggplot(deltas, aes(delta_3)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = deltas$delta_3, "pnorm", mean(deltas$delta_3), sd(deltas$delta_3))



#### DELTA 4
summary(deltas$delta_4)
quantile(deltas$delta_4)
sd(deltas$delta_4)
sd(deltas$delta_4)/mean(deltas$delta_4)

# outliers
boxplot(deltas$delta_4, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(deltas$delta_4)))
ggplot(deltas, aes(delta_4)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = deltas$delta_4, "pnorm", mean(deltas$delta_4), sd(deltas$delta_4))




#### DELTA 5
summary(deltas$delta_5)
quantile(deltas$delta_5)
sd(deltas$delta_5)
sd(deltas$delta_5)/mean(deltas$delta_5)

# outliers
boxplot(deltas$delta_5, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(deltas$delta_5)))
ggplot(deltas, aes(delta_5)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = deltas$delta_5, "pnorm", mean(deltas$delta_5), sd(deltas$delta_5))


#### TEST WILCOXON DE MEDIANAS PARA DELTA 1 Y 5

wilcox.test(x = deltas$delta_1, y = deltas$delta_5, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)




################
## PREGUNTA 2 ##
################

### delta 2

# test turno 1
tuno1_delta2 <- data %>% filter(turno == 'T1') %>% select(delta_2)
ks.test(x = tuno1_delta2$delta_2, "pnorm", 
        mean(tuno1_delta2$delta_2), sd(tuno1_delta2$delta_2))

# test turno 2
tuno2_delta2 <- data %>% filter(turno == 'T2') %>% select(delta_2)
ks.test(x = tuno2_delta2$delta_2, "pnorm", 
        mean(tuno2_delta2$delta_2), sd(tuno2_delta2$delta_2))

# test turno 1
tuno3_delta2 <- data %>% filter(turno == 'T3') %>% select(delta_2)
ks.test(x = tuno3_delta2$delta_2, "pnorm", 
        mean(tuno3_delta2$delta_2), sd(tuno3_delta2$delta_2))

# boxplots
ggplot(data = data, 
       mapping = aes(turno, delta_2, colour = turno)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Turnos") + ylab("Minutos") +
  ylim(0,3.2)

# turno 1 y 2
wilcox.test(x = tuno1_delta2$delta_2, y = tuno2_delta2$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

# turno 1 y 3
wilcox.test(x = tuno1_delta2$delta_2, y = tuno3_delta2$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

# turno 2 y 3
wilcox.test(x = tuno2_delta2$delta_2, y = tuno3_delta2$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


################
## PREGUNTA 4 ##
################

table(data$tipo_carga, data$delta_1)

tbl <- table(data$turno, data$tipo_carga)
prop.table(tbl)

mosaicplot(tbl,main="observada",las=2)

chisq.test(x = tbl)



################
## PREGUNTA 5 ##
################

#### Alimento no perecible
categ_alimentos_delta <- data %>% 
  filter(tipo_carga == 'Alimentos no perecibles') %>% 
  select(delta_2, delta_3, delta_4)

# delta 2
median(categ_alimentos_delta$delta_2)
# delta 3
median(categ_alimentos_delta$delta_3)
# delta 4
median(categ_alimentos_delta$delta_4)


#### Medicamentos
categ_medic_delta <- data %>% 
  filter(tipo_carga == 'Medicamentos') %>% 
  select(delta_2, delta_3, delta_4)

# delta 2
median(categ_medic_delta$delta_2)
# delta 3
median(categ_medic_delta$delta_3)
# delta 4
median(categ_medic_delta$delta_4)

#### Texto
categ_texto_delta <- data %>% 
  filter(tipo_carga == 'Textos ') %>% 
  select(delta_2, delta_3, delta_4)

# delta 2
median(categ_texto_delta$delta_2)
# delta 3
median(categ_texto_delta$delta_3)
# delta 4
median(categ_texto_delta$delta_4)


#### Test Wilcoxon --- Delta 2
wilcox.test(x = categ_alimentos_delta$delta_2, y = categ_medic_delta$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_delta$delta_2, y = categ_texto_delta$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_delta$delta_2, y = categ_texto_delta$delta_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

#### Test Wilcoxon --- Delta 3
wilcox.test(x = categ_alimentos_delta$delta_3, y = categ_medic_delta$delta_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_delta$delta_3, y = categ_texto_delta$delta_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_delta$delta_3, y = categ_texto_delta$delta_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


#### Test Wilcoxon --- Delta 4
wilcox.test(x = categ_alimentos_delta$delta_4, y = categ_medic_delta$delta_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_delta$delta_4, y = categ_texto_delta$delta_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_delta$delta_4, y = categ_texto_delta$delta_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


################
## PREGUNTA 6 ##
################

# grafico de correlacion de spearman
M <- cor(deltas[c(2,3,4)], method="spearman") 
corrplot(M, method = "ellipse", type = "full")

# teste de correlacion por metodo spearman
cor.test(deltas$delta_2, deltas$delta_3, method = "spearman")
cor.test(deltas$delta_2, deltas$delta_4, method = "spearman")
cor.test(deltas$delta_3, deltas$delta_4, method = "spearman")



##################################
##################################
##################################

##### LAS OTRAS VARIABLES

ggplot(data_sna, aes(tipo_carga)) + 
  geom_histogram(stat="count") +
  xlab("Cargamento") + ylab("Cantidad") +
  theme_minimal() 

ggplot(data_sna, aes(turno)) + 
  geom_histogram(stat="count") +
  xlab("Turno") + ylab("Cantidad") +
  theme_minimal() 
