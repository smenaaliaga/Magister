setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 1 - 2021")

#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("corrplot")

library(tidyverse)
library(reshape2)
library(corrplot)
require(MASS)


#############################
## TRANSFORMACION DE DATOS ##
#############################

original <- read.csv2("Base_T1_modif.csv")

data <- original %>%
     select(
         tiempo_1 = delta_1,
         tiempo_2 = delta_2,
         responsable = Responsable,
         tipo_carga = Tipo_carga,
         tiempo_3 = delta_3,
         responsable_O1 = Responsable_O1,
         tiempo_4 = delta_4,
         responsable_O2 = Responsable_O2,
         tiempo_5 = delta_5
         ) 

# SE CONVIERTEN LOS tiempos EN NUMERICOS
data <- data %>% mutate(
       tiempo_1 = as.numeric(tiempo_1),
       tiempo_2 = as.numeric(tiempo_2),
       tiempo_3 = as.numeric(tiempo_3),
       tiempo_4 = as.numeric(tiempo_4),
       tiempo_5 = as.numeric(tiempo_5),
       tipo_carga = as.factor(tipo_carga),
       responsable = as.factor(responsable),
       responsable_O1 = as.factor(responsable_O1),
       responsable_O2 = as.factor(responsable_O2),
     )

################
## Pregunta 1 ##
################
# ?ggplot

#### tiempo 1
# resumen
summary(data$tiempo_1)
quantile(data$tiempo_1)
sd(data$tiempo_1)
sd(data$tiempo_1)/mean(tiempos$tiempo_1)

# outliers
boxplot(data$tiempo_1, plot=FALSE)$out

# boxplot sin correcion de NA
ceiling(1 + 3.322 * log10(length(data$tiempo_1)))

ggplot(data, aes(data$tiempo_1)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  xlim(0,8)

# correción NA #?mean
data_corr <- data %>% 
  mutate(tiempo_1 = replace_na(tiempo_1,median(data$tiempo_1, na.rm = TRUE)))

# boxplot con correcion de NA
ggplot(data_corr, aes(data_corr$tiempo_1)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  xlim(0,7.5)

# resumen
summary(data_corr$tiempo_1)
quantile(data_corr$tiempo_1)
sd(data_corr$tiempo_1)
sd(data_corr$tiempo_1)/mean(data_corr$tiempo_1)

# test k-s para exponencial
estimate_tiempo_1 <- fitdistr(data_corr$tiempo_1, "exponential")$estimate[1]
ks.test(x = data_corr$tiempo_1,"pexp",estimate_tiempo_1)




#### tiempo 2

# outliers
boxplot(data$tiempo_2, plot=FALSE)$out

# Remplazar dato negativo por NA y luego su mediana
data_corr <- data_corr %>% 
  mutate(tiempo_2 = replace(tiempo_2, tiempo_2 < 0, NA))

data_corr <- data_corr %>% 
  mutate(tiempo_2 = replace_na(tiempo_2,median(data$tiempo_2, na.rm = TRUE)))



# resumen
summary(data_corr$tiempo_2)
quantile(data_corr$tiempo_2)
sd(data_corr$tiempo_2)
sd(data_corr$tiempo_2)/mean(data_corr$tiempo_2)


# boxplot
ceiling(1 + 3.322 * log10(length(data_corr$tiempo_2)))
ggplot(data_corr, aes(tiempo_2)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  xlim(0,5)



Ajustex <- fitdistr(data_corr$tiempo_2, "gamma")
Ajustex$estimate[1]
Ajustex$estimate[2]
ks.test(data_corr$tiempo_2, "pgamma", Ajustex$estimate[1], Ajustex$estimate[2])



install.packages("fitdistrplus")
install.packages("logspline")

library(fitdistrplus)
library(logspline)

# logistica
fit.logis_2 <- fitdist(data_corr$tiempo_2, "logis")
plot(fit.logis_2)

est.logis_2 <- fitdistr(data_corr$tiempo_2, "logistic")
ks.test(data_corr$tiempo_2, "plogis", est.logis$estimate[1], est.logis$estimate[2])


uuuh


####### TIEMPO 2 - OUTLIERS ELIMINADOS

t2o <- data_corr$tiempo_2[!data_corr$tiempo_2 %in% boxplot.stats(data_corr$tiempo_2)$out]

# "norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif" and "logis"

# gamma
fit.gamma <- fitdist(t2o, "gamma")
plot(fit.gamma)

est.gamma <- fitdistr(t2o, "gamma")
ks.test(t2o, "pgamma", est.gamma$estimate[1], est.gamma$estimate[2])

# exponencial
fit.exp <- fitdist(t2o, "exp")
plot(fit.exp)

est.exp <- fitdistr(t2o, "exponential")
ks.test(t2o, "pexp", est.exp$estimate[1])

# normal
fit.norm <- fitdist(t2o, "norm")
plot(fit.norm)

est.normal <- fitdistr(t2o, "normal")
ks.test(t2o, "pnorm", est.normal$estimate[1], est.normal$estimate[2])
  
# log normal
fit.lnorm <- fitdist(t2o, "lnorm")
plot(fit.lnorm)

est.lnormal <- fitdistr(t2o, "lnormal")
ks.test(t2o, "plnorm", est.lnormal$estimate[1], est.lnormal$estimate[2])

# logistica
fit.logis <- fitdist(t2o, "logis")
plot(fit.logis)

est.logis <- fitdistr(t2o, "logistic")
ks.test(t2o, "plogis", est.logis$estimate[1], est.logis$estimate[2])



df <- data.frame(t2o)

ceiling(1 + 3.322 * log10(length(df$t2o)))
ggplot(df, aes(t2o)) +
  geom_histogram(bins = 16, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() +
  stat_function(fun = function(x) 
  {dlogis(x, est.logis$estimate[1], est.logis$estimate[2]) * 250}, 
  aes(colour = "Dist. Logística"), size = 1) 


ggplot(v, aes(x = value, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  geom_histogram(aes(y = ..count..), 
                 breaks = b,
                 binwidth = binwidth,  
                 colour = "black", 
                 fill = "white") +
  geom_line(aes(y = ..density.. * 1226 * 12, colour = "Empirical"),
            size = 1, stat = 'density') +
  stat_function(fun = function(x) 
  {dnorm(x, mean = mean, sd = sd) * n * binwidth}, 
  aes(colour = "Normal"), size = 1) 
+
  labs(x = "Score", y = "Frequency") +
  scale_colour_manual(name = "Line colors", values = c("red", "blue"))

?dlogis

#### tiempo 3
# resumen
summary(tiempos$tiempo_3)
quantile(tiempos$tiempo_3)
sd(tiempos$tiempo_3)
sd(tiempos$tiempo_3)/mean(tiempos$tiempo_3)

# outliers
boxplot(tiempos$tiempo_3, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(tiempos$tiempo_3)))
ggplot(tiempos, aes(tiempo_3)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = tiempos$tiempo_3, "pnorm", mean(tiempos$tiempo_3), sd(tiempos$tiempo_3))



#### tiempo 4
summary(tiempos$tiempo_4)
quantile(tiempos$tiempo_4)
sd(tiempos$tiempo_4)
sd(tiempos$tiempo_4)/mean(tiempos$tiempo_4)

# outliers
boxplot(tiempos$tiempo_4, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(tiempos$tiempo_4)))
ggplot(tiempos, aes(tiempo_4)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = tiempos$tiempo_4, "pnorm", mean(tiempos$tiempo_4), sd(tiempos$tiempo_4))




#### tiempo 5
summary(tiempos$tiempo_5)
quantile(tiempos$tiempo_5)
sd(tiempos$tiempo_5)
sd(tiempos$tiempo_5)/mean(tiempos$tiempo_5)

# outliers
boxplot(tiempos$tiempo_5, plot=FALSE)$out

# boxplot
ceiling(1 + 3.322 * log10(length(tiempos$tiempo_5)))
ggplot(tiempos, aes(tiempo_5)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# test normalidad
ks.test(x = tiempos$tiempo_5, "pnorm", mean(tiempos$tiempo_5), sd(tiempos$tiempo_5))


#### BOXPLOT para tiempos
tiempos <- data %>% select(tiempo_1, tiempo_2, tiempo_3, tiempo_4, tiempo_5)

# ?melt
tiempos_fact <- melt(tiempos)

ggplot(data = tiempos_fact, 
       mapping = aes(variable, value, colour = variable)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("tiempos") + ylab("Minutos") +
  ylim(0,84)

summary(tiempos)

boxplot(tiempos, ylim=c(0,65))



#### TEST WILCOXON DE MEDIANAS PARA tiempo 1 Y 5

wilcox.test(x = tiempos$tiempo_1, y = tiempos$tiempo_5, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)




################
## PREGUNTA 2 ##
################

### tiempo 2

# test turno 1
tuno1_tiempo2 <- data %>% filter(turno == 'T1') %>% select(tiempo_2)
ks.test(x = tuno1_tiempo2$tiempo_2, "pnorm", 
        mean(tuno1_tiempo2$tiempo_2), sd(tuno1_tiempo2$tiempo_2))

# test turno 2
tuno2_tiempo2 <- data %>% filter(turno == 'T2') %>% select(tiempo_2)
ks.test(x = tuno2_tiempo2$tiempo_2, "pnorm", 
        mean(tuno2_tiempo2$tiempo_2), sd(tuno2_tiempo2$tiempo_2))

# test turno 1
tuno3_tiempo2 <- data %>% filter(turno == 'T3') %>% select(tiempo_2)
ks.test(x = tuno3_tiempo2$tiempo_2, "pnorm", 
        mean(tuno3_tiempo2$tiempo_2), sd(tuno3_tiempo2$tiempo_2))

# boxplots
ggplot(data = data, 
       mapping = aes(turno, tiempo_2, colour = turno)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Turnos") + ylab("Minutos") +
  ylim(0,3.2)

# turno 1 y 2
wilcox.test(x = tuno1_tiempo2$tiempo_2, y = tuno2_tiempo2$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

# turno 1 y 3
wilcox.test(x = tuno1_tiempo2$tiempo_2, y = tuno3_tiempo2$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

# turno 2 y 3
wilcox.test(x = tuno2_tiempo2$tiempo_2, y = tuno3_tiempo2$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


################
## PREGUNTA 4 ##
################

table(data$tipo_carga, data$tiempo_1)

tbl <- table(data$turno, data$tipo_carga)
prop.table(tbl)

mosaicplot(tbl,main="observada",las=2)

chisq.test(x = tbl)



################
## PREGUNTA 5 ##
################

#### Alimento no perecible
categ_alimentos_tiempo <- data %>% 
  filter(tipo_carga == 'Alimentos no perecibles') %>% 
  select(tiempo_2, tiempo_3, tiempo_4)

# tiempo 2
median(categ_alimentos_tiempo$tiempo_2)
# tiempo 3
median(categ_alimentos_tiempo$tiempo_3)
# tiempo 4
median(categ_alimentos_tiempo$tiempo_4)


#### Medicamentos
categ_medic_tiempo <- data %>% 
  filter(tipo_carga == 'Medicamentos') %>% 
  select(tiempo_2, tiempo_3, tiempo_4)

# tiempo 2
median(categ_medic_tiempo$tiempo_2)
# tiempo 3
median(categ_medic_tiempo$tiempo_3)
# tiempo 4
median(categ_medic_tiempo$tiempo_4)

#### Texto
categ_texto_tiempo <- data %>% 
  filter(tipo_carga == 'Textos ') %>% 
  select(tiempo_2, tiempo_3, tiempo_4)

# tiempo 2
median(categ_texto_tiempo$tiempo_2)
# tiempo 3
median(categ_texto_tiempo$tiempo_3)
# tiempo 4
median(categ_texto_tiempo$tiempo_4)


#### Test Wilcoxon --- tiempo 2
wilcox.test(x = categ_alimentos_tiempo$tiempo_2, y = categ_medic_tiempo$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_tiempo$tiempo_2, y = categ_texto_tiempo$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_tiempo$tiempo_2, y = categ_texto_tiempo$tiempo_2, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)

#### Test Wilcoxon --- tiempo 3
wilcox.test(x = categ_alimentos_tiempo$tiempo_3, y = categ_medic_tiempo$tiempo_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_tiempo$tiempo_3, y = categ_texto_tiempo$tiempo_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_tiempo$tiempo_3, y = categ_texto_tiempo$tiempo_3, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


#### Test Wilcoxon --- tiempo 4
wilcox.test(x = categ_alimentos_tiempo$tiempo_4, y = categ_medic_tiempo$tiempo_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_alimentos_tiempo$tiempo_4, y = categ_texto_tiempo$tiempo_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)
wilcox.test(x = categ_medic_tiempo$tiempo_4, y = categ_texto_tiempo$tiempo_4, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)


################
## PREGUNTA 6 ##
################

# grafico de correlacion de spearman
M <- cor(tiempos[c(2,3,4)], method="spearman") 
corrplot(M, method = "ellipse", type = "full")

# teste de correlacion por metodo spearman
cor.test(tiempos$tiempo_2, tiempos$tiempo_3, method = "spearman")
cor.test(tiempos$tiempo_2, tiempos$tiempo_4, method = "spearman")
cor.test(tiempos$tiempo_3, tiempos$tiempo_4, method = "spearman")



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
