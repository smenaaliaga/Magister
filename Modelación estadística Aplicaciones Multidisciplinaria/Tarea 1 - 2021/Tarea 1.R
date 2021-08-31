setwd("C:/Users/smena/Google Drive/Magister/Modelación estadística Aplicaciones Multidisciplinaria/Tarea 1 - 2021")

# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("corrplot")
# install.packages("fitdistrplus")
# install.packages("logspline")
# install.packages("goftest")
# install.packages("car")

library(tidyverse)
library(reshape2)
library(corrplot)
require(MASS)
library(fitdistrplus)
library(logspline)
library(goftest)
library(car)


#############################
## TRANSFORMACION DE DATOS ##
#############################

original <- read.csv2("Base_T1_modif.csv")

# SE CONVIERTEN LOS tiempos EN NUMERICOS, lo demás en factor
data <- original %>%
    dplyr::select(
      tiempo_1 = delta_1,
      tiempo_2 = delta_2,
      responsable = Responsable,
      tipo_carga = Tipo_carga,
      tiempo_3 = delta_3,
      responsable_O1 = Responsable_O1,
      tiempo_4 = delta_4,
      responsable_O2 = Responsable_O2,
      tiempo_5 = delta_5,
      turno = Turno
    ) %>% mutate(
       tiempo_1 = as.numeric(tiempo_1),
       tiempo_2 = as.numeric(tiempo_2),
       tiempo_3 = as.numeric(tiempo_3),
       tiempo_4 = as.numeric(tiempo_4),
       tiempo_5 = as.numeric(tiempo_5),
       turno = as.factor(turno),
       tipo_carga = as.factor(tipo_carga),
       responsable = as.factor(responsable),
       responsable_O1 = as.factor(responsable_O1),
       responsable_O2 = as.factor(responsable_O2),
     )

################
## Pregunta 1 ##
################
# ?ggplot

##### tiempo 1
###############

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

# resumen t1 con datos corregidos
summary(data_corr$tiempo_1)
quantile(data_corr$tiempo_1)
sd(data_corr$tiempo_1)
sd(data_corr$tiempo_1)/mean(data_corr$tiempo_1)

# graficos bondad de ajuste
fit.t_2 <- fitdist(data_corr$tiempo_1, "exp")
plot(fit.t_2)

# test k-s para exponencial
est.t_2 <- fitdistr(data_corr$tiempo_2, "exp")
ks.test(data_corr$tiempo_2, "pexp", est.t_2$estimate[1], est.t_2$estimate[2])

### TIEMPO 1 - BONDAD DE AJUSTE SIN OUTLIERS 
boxplot.stats(data_corr$tiempo_1)$out
t1o <- data_corr$tiempo_1[!data_corr$tiempo_1 %in% boxplot.stats(data_corr$tiempo_1)$out]

# graficos bondad de ajuste sin outliers
fit.t_1 <- fitdist(t1o, "exp")
plot(fit.t_1, plotstyle = "ggplot")

plot(fit.t_1, histo = FALSE, demp = TRUE, title = FALSE)


cdfcomp(fit.t_1, addlegend=FALSE)
denscomp(fit.t_1, plotstyle = "ggplot", main = "") + ggplot2::theme_minimal()
ppcomp(fit.t_1, addlegend=FALSE)
qqcomp(fit.t_1, addlegend=FALSE)

# test k-s para exponencial, muestra sin outliers
est.t_1 <- fitdistr(t1o, "exponential")
ks.test(t2o, "pexp", est.t_1$estimate[1])





##### tiempo 2
###############

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



out_t2 <- boxplot.stats(data_corr$tiempo_2)$out
df_out_t2 <- as.data.frame(out_t2)

ggplot(df_out_t2, aes(out_t2)) +
  geom_histogram(bins = 50, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal()  +
  xlim(0,100)



Ajustex <- fitdistr(data_corr$tiempo_2, "gamma")
Ajustex$estimate[1]
Ajustex$estimate[2]
ks.test(data_corr$tiempo_2, "pgamma", Ajustex$estimate[1], Ajustex$estimate[2])





# logistica
fit.logis_2 <- fitdist(data_corr$tiempo_2, "logis")
plot(fit.logis_2)

est.logis_2 <- fitdistr(data_corr$tiempo_2, "logistic")
ks.test(data_corr$tiempo_2, "plogis", est.logis$estimate[1], est.logis$estimate[2])



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
denscomp(fit.logis, plotstyle = "ggplot", demp = TRUE, main = "") + ggplot2::theme_minimal()

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
#############

# outliers
boxplot(data_corr$tiempo_3, plot=FALSE)$out

# resumen
summary(data_corr$tiempo_3)
quantile(data_corr$tiempo_3)
sd(data_corr$tiempo_3)
sd(data_corr$tiempo_3)/mean(data_corr$tiempo_3)

# boxplot
ceiling(1 + 3.322 * log10(length(data_corr$tiempo_3)))
ggplot(data_corr, aes(tiempo_3)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 


# graficos bondad de ajuste
fit.t_3 <- fitdist(data_corr$tiempo_3, "exp")
plot(fit.t_3)

denscomp(fit.t_3, plotstyle = "ggplot", demp = TRUE, main = "") + ggplot2::theme_minimal()

# test k-s para exponencial, muestra sin outliers
est.t_3 <- fitdistr(data_corr$tiempo_3, "exponential")
ks.test(data_corr$tiempo_3, "pexp", est.t_3$estimate[1])




#### tiempo 4
#############

# outliers
boxplot(data_corr$tiempo_4, plot=FALSE)$out

summary(data_corr$tiempo_4)
quantile(data_corr$tiempo_4)
sd(data_corr$tiempo_4)
sd(data_corr$tiempo_4)/mean(data_corr$tiempo_4)

# boxplot
ceiling(1 + 3.322 * log10(length(data_corr$tiempo_4)))
  ggplot(data_corr, aes(tiempo_4)) +
    geom_histogram(bins = 12, color="white", boundary=0) +
    xlab("Tiempos [minuto]") + ylab("Frecuencia") +
    theme_minimal() 


# graficos bondad de ajuste
fit.t_4 <- fitdist(data_corr$tiempo_4, "exp")
plot(fit.t_4)

denscomp(fit.t_4, plotstyle = "ggplot", demp = TRUE, main = "") + ggplot2::theme_minimal()

# test k-s para exponencial, muestra sin outliers
est.t_4 <- fitdistr(data_corr$tiempo_4, "exponential")
ks.test(data_corr$tiempo_4, "pexp", est.t_4$estimate[1])

est.t_4 <- fitdistr(data_corr$tiempo_4, "exponential")
ad.test(data_corr$tiempo_4, "pexp", est.t_4$estimate[1], est.t_4$estimate[2])


#### tiempo 5
#############

# outliers
boxplot(data_corr$tiempo_5, plot=FALSE)$out

summary(data_corr$tiempo_5)
quantile(data_corr$tiempo_5)
sd(data_corr$tiempo_5)
sd(data_corr$tiempo_5)/mean(data_corr$tiempo_5)


# boxplot
ceiling(1 + 3.322 * log10(length(tiempos$tiempo_5)))
ggplot(tiempos, aes(tiempo_5)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 

# graficos bondad de ajuste
fit.t_5 <- fitdist(data_corr$tiempo_5, "exp")
plot(fit.t_5)

denscomp(fit.t_5, plotstyle = "ggplot", demp = TRUE, main = "") + ggplot2::theme_minimal()

# test k-s para exponencial, muestra sin outliers
est.t_5 <- fitdistr(data_corr$tiempo_5, "exponential")
ks.test(data_corr$tiempo_5, "pexp", est.t_5$estimate[1])


#### tipo de ciclo
##################

tiempo_ciclo <- data_corr$tiempo_1 + data_corr$tiempo_2 + data_corr$tiempo_3 + data_corr$tiempo_4 + data_corr$tiempo_5
tiempo_ciclo <- as.data.frame(tiempo_ciclo)

# outliers
boxplot(tiempo_ciclo$tiempo_ciclo, plot=FALSE)$out

# correcion de outliers
tc_outliers <- tiempo_ciclo$tiempo_ciclo[!tiempo_ciclo$tiempo_ciclo %in% boxplot.stats(tiempo_ciclo$tiempo_ciclo)$out]
tc_outliers <- as.data.frame(tc_outliers)

# resumen
summary(tc_outliers$tc_outliers)
quantile(tc_outliers$tc_outliers)
sd(tc_outliers$tc_outliers)
sd(tc_outliers$tc_outliers)/mean(tc_outliers$tc_outliers)

# boxplot
ceiling(1 + 3.322 * log10(length(tiempo_ciclo$tc_outliers)))
ggplot(tc_outliers, aes(tc_outliers)) +
  geom_histogram(bins = 12, color="white", boundary=0) +
  xlab("Tiempos [minuto]") + ylab("Frecuencia") +
  theme_minimal() 


# graficos bondad de ajuste
fit.t_c <- fitdist(tc_outliers$tc_outliers, "exp")
plot(fit.t_c)

denscomp(fit.t_c, plotstyle = "ggplot", demp = TRUE, main = "") + ggplot2::theme_minimal()

# test k-s para exponencial, muestra sin outliers
est.t_c <- fitdistr(tc_outliers$tc_outliers, "exponential")
ks.test(tc_outliers$tc_outliers, "pexp", est.t_c$estimate[1])



#### tipo de carga
##################

# outliers
boxplot(data_corr$tipo_carga, plot=FALSE)$out
summary(data_corr$tipo_carga)

ggplot(data_corr, aes(tipo_carga)) +
  geom_bar(size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Tipo de carga") + ylab("Cantidad")


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



#### test WILCOXON para diferencias de tiempos 1 Y 5

est.normal <- fitdistr(t2o, "normal")
ks.test(t2o, "pnorm", est.normal$estimate[1], est.normal$estimate[2])

est.normal <- fitdistr(data_corr$tiempo_5, "normal")
ks.test(data_corr$tiempo_5, "pnorm", est.normal$estimate[1], est.normal$estimate[2])

wilcox.test(x = t1o, y = data_corr$tiempo_5, 
            alternative = "two.sided", mu = 0, paired = F,
            conf.int = 0.95)




################
## PREGUNTA 2 ##
################

rm(turno3_t2)

# test turno 1
turno1_t2 <- data_corr %>% filter(turno == 'T1') %>% dplyr::select(tiempo_2)
summary(turno1_t2)
nrow(turno1_t2)
ks.test(turno1_t2$tiempo_2, "pnorm", 
        mean(turno1_t2$tiempo_2), sd(turno1_t2$tiempo_2))

# test turno 2
turno2_t2 <- data_corr %>% filter(turno == 'T2') %>% dplyr::select(tiempo_2)
summary(turno2_t2)
nrow(turno2_t2)
ks.test(turno2_t2$tiempo_2, "pnorm", 
        mean(turno2_t2$tiempo_2), sd(turno2_t2$tiempo_2))

# test turno 3
turno3_t2 <- data_corr %>% filter(turno == 'T3') %>% dplyr::select(tiempo_2)
summary(turno3_t2)
nrow(turno3_t2)
ks.test(turno3_t2$tiempo_2, "pnorm", 
        mean(turno3_t2$tiempo_2), sd(turno3_t2$tiempo_2))

# boxplots
ggplot(data = data_corr, 
       mapping = aes(turno, tiempo_2, colour = turno)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Turnos") + ylab("Minutos") +
  ylim(0,3.2)


leveneTest(tiempo_2 ~ turno, data = data_corr, center = "median")

?leveneTest

# Test Kruskal-Wallis
kruskal.test(tiempo_2 ~ turno, data = data_corr)



################
## PREGUNTA 3 ##
################

vis_miss()



################
## PREGUNTA 4 ##
################

tbl <- table(data_corr$turno, data_corr$tipo_carga)
prop.table(tbl)

?table

mosaicplot(tbl,main="observada",las=2)

chisq.test(x = tbl)


tbl_2 <- table(data_corr$tipo_carga, data_corr$turno)


mosaicplot(tbl_2)

chisq.test(tbl) 



prueba <- data_corr %>% 
  filter(tipo_carga == "Textos " | tipo_carga == "Textil") %>% 
  dplyr::select(turno, tipo_carga)
prueba$tipo_carga <- factor(prueba$tipo_carga)
levels(prueba$tipo_carga)
tbl_p <- table(prueba$turno, prueba$tipo_carga)
prop.table(tbl_p)

chisq.test(tbl_p) 



bienes_lujo <- data_corr %>% filter(tipo_carga == 'Bienes de lujo')
nrow(bienes_lujo)


fisher.test(x = tbl, simulate.p.value=TRUE)

?fisher.test


############ P4.2

# boxplots
ggplot(data = data_corr, 
       mapping = aes(tipo_carga, 
                     tiempo_1 + tiempo_2 + tiempo_3 + tiempo_4 + tiempo_5, 
                     colour = tipo_carga)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Tipo de carga") + ylab("Minutos") + 
  ylim(0,130)



leveneTest(tiempo_1 + tiempo_2 + tiempo_3 + tiempo_4 + tiempo_5 ~ tipo_carga, 
           data = data_corr, center = "median")


summary(data_corr$tiempo_1)

?summery

?leveneTest

# Test Kruskal-Wallis
kruskal.test(tiempo_1 + tiempo_2 + tiempo_3 + tiempo_4 + tiempo_5 ~ tipo_carga, data = data_corr)



################
## PREGUNTA 5 ##
################

cor.test(data_corr$tiempo_1, data_corr$tiempo_3, method = "spearman")

cor.test(data_corr$tiempo_1, data_corr$tiempo_4, method = "spearman")

cor.test(data_corr$tiempo_3, data_corr$tiempo_4, method = "spearman")


ggplot(data_corr, aes(tiempo_3, tiempo_4)) + geom_point() + xlim(0,50)



################
## PREGUNTA 6 ##
################


library(reshape2)
dat.m <- melt(data_corr, 
              measure.vars=c('tiempo_1','tiempo_2','tiempo_3','tiempo_4','tiempo_5'))

mean(data_corr$tiempo_1)
mean(data_corr$tiempo_2)
mean(data_corr$tiempo_3)
mean(data_corr$tiempo_4)
mean(data_corr$tiempo_5)


# boxplots
ggplot(data = dat.m, 
       mapping = aes(variable, value, colour = variable)) +
  geom_boxplot(outlier.shape=NA, size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Estaciones") + ylab("Minutos") + 
  ylim(0,85)



# Test Kruskal-Wallis
krusk.test(data_corr$tiempo_4 , data_corr$tiempo_2)

# Test Kruskal-Wallis
ks.test(data_corr$tiempo_4 , data_corr$tiempo_2, alternative = "greater")

  ?kruskal.test
?ks.test
