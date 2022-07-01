# install.packages("clusterCrit")
# install.packages("cluster")
library(clusterCrit)
library(ggplot2)
library(tidyverse)

iris_filter<- iris %>% select(-Species)
# ESTANDARIZACIÓN NORMAL
iris_filter<-scale(iris_filter)
# NÚMERO MÍNIMO DE CLUSTERS
min_nc=2
# NÚMERO MÁXIMO DE CLUSTER
max_nc=8
# CONSTRUCCIÓN DE MATRIZ NULA
evaluacion <- array(0, c(max_nc-min_nc+1, 4))
# AGREGAR NÚMERO DE CLUSTERS A FORMAR
evaluacion[,1] <- min_nc:max_nc
# CALCULO DE LAS MÉTRICAS SILHOUETTE, DUNN Y DAVIES BOULDIN
# CONSTRUCCIÓN DE UN PROCESO ITERATIVO
for (nc in min_nc:max_nc){
  cl2 <- kmeans(iris_filter,nc)
  aux<-intCriteria(iris_filter,cl2$cluster,c("Dunn","Silhouette","Davies_Bouldin"))
  # AGREGAR EL ÍNDICE DE DUNN EN LA COLUMNA 2
  evaluacion[nc-min_nc+1, 2] <- D <-aux$dunn
  # AGREGAR EL ÍNDICE DE SILHOUETTE EN LA COLUMNA 3
  evaluacion[nc-min_nc+1, 3] <- S <-aux$silhouette
  # AGREGAR EL ÍNDICE DE DAVIES BOULDIN EN LA COLUMNA 4
  evaluacion[nc-min_nc+1, 4] <- DB <-aux$davies_bouldin
}
# CAMBIO DE NOMBRE DE LA COLUMNAS DE LA MATRIZ
colnames(evaluacion)<-c("k","Dunn","Silhouette","Davies_Bouldin")
print(evaluacion)

# CALCULAR EL ÍNDICE DE LA SILHOUETTE PARA CADA OBSERVACIÓN
library(cluster)
sil_ind <- silhouette(kmeans(x=iris_filter,centers = 3)$cluster,dist(iris_filter))
sil_ind_df <- sil_ind[1:nrow(iris_filter),] %>% data.frame
# AGRUPAMOS POR CLUSTER Y CALCULAMOS LA MEDIA DEL ÍNDICE DE LA SILHOUETTE
# ESTO NOS PERMITE IDENTIFICAR AQUELLOS GRUPOS MAL FORMADOS
sil_ind_df %>% group_by(cluster) %>%
            summarise(mean_sil_width = mean(sil_width)) %>%
            arrange(cluster)
