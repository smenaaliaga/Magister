library(tidyverse)
# MATRIZ DE DISTANCIAS
d <- matrix(c(0,0,0,0,0,
              3,0,0,0,0,
              3,6,0,0,0,
              2,7,6,0,0,
              5,4,7,6,0),
            ncol=5,
            byrow = T) %>% as.dist()

plot(hclust(d,method="average"))

# DATASET IRIS
iris %>% head

# MUESTRA DE DATOS ALEATORIOS

idx <- sample(1:nrow(iris),nrow(iris)*0.4)
iris_filter <- iris[idx,]

# DISTANCIA DE LAS 4 FEATURES DEL CONJUNTO IRIS
d <- iris_filter %>% select(-Species) %>%
  scale %>% 
  data.frame %>% dist

# MEDIDAS DE DISIMILITUD 
cluster <- hclust(d,method="complete") # "ward.D","ward.D2","single","complete","average","mcquitty","median","centroid"

# OBTENCIÓN DE 3 CLUSTERS
grupos <- cutree(cluster,3)
grupos

plot(cluster,labels=iris_filter$Species)
iris_filter$jerarquico <- grupos
table(iris_filter$Species,iris_filter$jerarquico)

# GRÁFICA DE LOS K GRUPOS FORMADOS
rect.hclust(cluster, k=3, border="red") 

library(factoextra)
# GRÁFICA DE LOS K GRUPOS FORMADOS
fviz_dend(x = cluster,k = 3, cex = 0.6) +
  geom_hline(yintercept = 5.5, 
             linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Linkage complete, K=3")

# MATRIZ DE DISTANCIA
a<- c(4,5,3,6,7)
b <- c(6,5,3,1,7)
dist(cbind(a,b),method="euclidean")

# VALIDACIÓN DE RESULTADOS DE LA MATRIZ DE DISTANCIAS
sqrt((4-5)^2 + (6-5)^2)

