library(tidyverse)
# MÉTODO DEL CODO
iris_filter <- iris %>% select(-Species)
k.max <- 10
# MEDIANTE FUNCIÓN OBETENEMOS EL ERROR TOTAL INTRA-CLUSTER
wss <- sapply(2:k.max, function(k){kmeans(iris_filter,
                                     k,
                                     nstart=50,
                                     iter.max = 15 )$tot.withinss})
# MEDIANTE CICLO FOR OBETENEMOS EL ERROR TOTAL INTRA-CLUSTER
wss <- NULL
for (k in 2:k.max){
  wss[k] <- kmeans(x = datos,
        center = k,
        nstart = 50,
        iter.max = 15)$tot.withinss
}
wss
# GRÁFICA DEL ERROR INTRA-CLUSTER PARA LOS DIFERENTES VALORES DE K (GRUPOS)
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="numero de clusters",
     ylab="SST dentro de los grupos",
     main = "Método del codo")
