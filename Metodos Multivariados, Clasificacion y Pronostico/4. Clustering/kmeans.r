library(tidyverse)

iris_filter <- iris %>% select(-Species)
label_iris <- iris%>% select(Species)
# PARTICIONAMIENTO JERARQUICO
# ALGORITMO DE K-MEANS
fit <- kmeans(iris_filter %>%
        select(-c(Species, jerarquico)),
        centers=3, # CANTIDAD DE GRUPOS A FORMAR
        nstart=50, 
        iter.max = 15)

fit$cluster # CLUSTERS FORMADOS
fit$totss # ERROR POR CLUSTER
sum(fit$withinss) # DISTANCIA INTRA-CLUSTER POR GRUPO FORMADO
fit$betweenss # DISTANCIA INTER-CLUSTER POR GRUPO FORMADO
fit$tot.withinss # DISTANCIA TOTAL INTRA-CLUSTER 
fit$size # TAMAÑO DE LOS GRUPOS FORMADOS
fit$iter # NÚMERO DE ITERACIONES

# VALIDACIÓN DE PERFORMANCE DE LOS CLUSTER FORMADO (NO ES UNA MÉTRICA, NORMALMENTE NO SE TIENE LA VARIABLE OBJETIVO, ESO SERÍA UN MODELO SUPERVISADO)
table(label_iris,fit$cluster)
