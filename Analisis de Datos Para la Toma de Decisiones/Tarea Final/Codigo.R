setwd("C:/Users/smena/Google Drive/Magister/ANALISIS DE DATOS PARA LA TOMA DE DECISIONES/Tarea Final")

library(tidyverse)

# Se agrupan las muestras en solo 2 columnas: identificador y dato
ColapsarMuestras <- function(dat){
  df <- tibble(x = "DELETE", y = 0)
  for(j in 1:ncol(dat)){
    for(i in 1:nrow(dat)){
      df <- df %>% add_row(x = colnames(dat)[j], y = dat[i,j])  
    }
  }
  df <- df[-1,]
  df
}

################
## PREGUNTA 1 ##
################

dat1 <- read.csv2("P3.csv")

##### PARTE 1 ######

### Kruskal - Wallis

# Se agrupan las muestras de planta en 2 columnas y se convierte a factor la planta
df <- ColapsarMuestras(dat1)

ggplot(data = df, mapping = aes(x = x, y = y, colour = x)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Plantas") + ylab("Kg") 

aggregate(y ~ x, data = df, FUN = median)

library(car)
leveneTest(y ~ x, data = df, center = "median")

kruskal.test(y ~ x, data = df)
?kruskal.test

##### PARTE 2 #####

shapiro.test(dat1$P1)
shapiro.test(dat1$P2)
shapiro.test(dat1$P3)
shapiro.test(dat1$P4)

?var.test
var.test(dat1$P1,dat1$P2)
var.test(dat1$P1,dat1$P3)
var.test(dat1$P1,dat1$P4)

var.test(dat1$P2,dat1$P3)
var.test(dat1$P2,dat1$P4)

var.test(dat1$P3,dat1$P4)


################
## Pregunta 2 ##
################

dat2 <- read.csv2("P4.csv")

###### PARTE 2 #####

summary(dat2)

df2 <- ColapsarMuestras(dat2)
  
ggplot(data = df2, mapping = aes(x = x, y = y, colour = x)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Medición") + ylab("Puntaje") 

?ks.test
#Test de normalidad K-S
ks.test(dat2$Satis, "pnorm", mean(dat2$Satis), sd(dat2$Satis))
ks.test(dat2$A1, "pnorm", mean(dat2$A1), sd(dat2$A1))
ks.test(dat2$A2, "pnorm", mean(dat2$A2), sd(dat2$A2))
ks.test(dat2$A3, "pnorm", mean(dat2$A3), sd(dat2$A3))
ks.test(dat2$A4, "pnorm", mean(dat2$A4), sd(dat2$A4))


library("nortest")
#Test de normalidad Lillefors
lillie.test(dat2$Satis)
lillie.test(dat2$A1)
lillie.test(dat2$A2)
lillie.test(dat2$A3)
lillie.test(dat2$A4)

?cor.test
# Coeficiente de correlación de Spearman
cor.test(dat2$A1, dat2$Satis, method = "spearman")
cor.test(dat2$A2, dat2$Satis, method = "spearman")
cor.test(dat2$A3, dat2$Satis, method = "spearman")
cor.test(dat2$A4, dat2$Satis, method = "spearman")

# Graficación de correlación
?cor()
M <- cor(dat2, method="spearman")

library(corrplot)
?corrplot
corrplot(M, method = "circle", type = "upper")


################
## Pregunta 3 ##
################

# Parte 2
ejemplo1 <- c(7,8,9,10,10,11,12,14,14,16,18,20)

?wilcox.test
wilcox.test(ejemplo1, mu = 10, alternative = "two.sided", 
            paired = F, conf.int = T, exact = F)

ejemplo2A <- c(3,2,4,8,6,13,6,11,16,9,7)
ejemplo2B <- c(8,17,15,8,19,11,7,9,11,20,4)

wilcox.test(ejemplo2A, ejemplo2B, mu =0, alternative = "two.sided", 
            paired = T,  exact = F)

################
## Pregunta 4 ##
################

dat4 <- read.csv2("P5.csv")

# Citerios a evaluar: 2, 3, 4, 5 y 6 

names(dat4)

dat4N1 <- dat4 %>% filter(AREA == 'N1') %>% select(TBP, HLM, EE, DI, TE)
dat4N2 <- dat4 %>% filter(AREA == 'N2') %>% select(TBP, HLM, EE, DI, TE)

###### PARTE 1 #####

# Resumen estadistico
summary(dat4N1)

sd(dat4N1$TBP, na.rm = T)
sd(dat4N1$HLM, na.rm = T)
sd(dat4N1$EE, na.rm = T)
sd(dat4N1$DI, na.rm = T)
sd(dat4N1$TE, na.rm = T)

summary(dat4N2)

sd(dat4N2$TBP, na.rm = T)
sd(dat4N2$HLM, na.rm = T)
sd(dat4N2$EE, na.rm = T)
sd(dat4N2$DI, na.rm = T)
sd(dat4N2$TE, na.rm = T)

dev.off()
ggplot(data = dat4, mapping = aes(x = AREA, y = TBP, colour = AREA)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Área") + ylab("Puntaje") 

ggplot(data = dat4, mapping = aes(x = AREA, y = HLM, colour = AREA)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Área") + ylab("Puntaje") 

ggplot(data = dat4, mapping = aes(x = AREA, y = EE, colour = AREA)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Área") + ylab("Puntaje") 

ggplot(data = dat4, mapping = aes(x = AREA, y = DI, colour = AREA)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Área") + ylab("Puntaje")

ggplot(data = dat4, mapping = aes(x = AREA, y = TE, colour = AREA)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Área") + ylab("Puntaje") 


###### PARTE 1 #####

?ks.test
# Test normalidad k-s grupo N1
ks.test(dat4N1$TBP , mean(dat4N1$TBP, na.rm = T), sd(dat4N1$TBP, na.rm = T))
ks.test(dat4N1$HLM , mean(dat4N1$HLM, na.rm = T), sd(dat4N1$HLM, na.rm = T))
ks.test(dat4N1$EE , mean(dat4N1$EE, na.rm = T), sd(dat4N1$EE, na.rm = T))
ks.test(dat4N1$DI , mean(dat4N1$DI, na.rm = T), sd(dat4N1$DI, na.rm = T))
ks.test(dat4N1$TE , mean(dat4N1$TE, na.rm = T), sd(dat4N1$TE, na.rm = T))

?sd

# Test normalidad k-s grupo N2
ks.test(dat4N2$TBP , mean(dat4N2$TBP, na.rm = T), sd(dat4N2$TBP, na.rm = T))
ks.test(dat4N2$HLM , mean(dat4N2$HLM, na.rm = T), sd(dat4N2$HLM, na.rm = T))
ks.test(dat4N2$EE , mean(dat4N2$EE, na.rm = T), sd(dat4N2$EE, na.rm = T))
ks.test(dat4N2$DI , mean(dat4N2$DI, na.rm = T), sd(dat4N2$DI, na.rm = T))
ks.test(dat4N2$TE , mean(dat4N2$TE, na.rm = T), sd(dat4N2$TE, na.rm = T))

# Test de diferencia de varianzas

?var.test
var.test(dat4N1$TBP, dat4N2$TBP, alternative = "two.sided")
var.test(dat4N1$HLM, dat4N2$HLM)
var.test(dat4N1$EE, dat4N2$EE)
var.test(dat4N1$DI, dat4N2$DI)
var.test(dat4N1$TE, dat4N2$TE)

# Test de diferencias de medias

?t.test
t.test(dat4N1$TBP, dat4N2$TBP, alternative = "two.sided", mu = 0, var.equal = TRUE)
t.test(dat4N1$HLM, dat4N2$HLM, alternative = "two.sided", mu = 0, var.equal = TRUE)
t.test(dat4N1$EE, dat4N2$EE, alternative = "two.sided", mu = 0, var.equal = TRUE)
t.test(dat4N1$DI, dat4N2$DI, alternative = "two.sided", mu = 0, var.equal = TRUE)
t.test(dat4N1$TE, dat4N2$TE, alternative = "two.sided", mu = 0, var.equal = TRUE)
