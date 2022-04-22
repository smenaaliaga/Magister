#---------------------------------------
#Sesion 1
#---------------------------------------
#tildes omitidos intencionalmente por fines de compatibilidad

#lectura de datos en R a partir de una base *.csv
dat=read.csv(file.choose(),dec=",",header=TRUE,sep=";")

#el objeto dat, la base de datos de ejemplo, quedara almacenada en R como un objeto Data Frame
class(dat)

#vista rapida de las 6 primeras filas de la base
head(dat)

#nombres de las variables en la base
names(dat)

#tipos de las variables en la base
class(dat$id)
class(dat$nombre)
class(dat$gen)
class(dat$N1)

#dat$N1 indica que se trabajara la variable N1 del objeto Data Frame llamado dat
#si se quiere trabajar solo con el nombre de las variables, se debe referenciar a ellas
#ejecutando previamente el siguiente comando:
#attach(dat)

#tabla de frecuencia absoluta para genero
table(dat$gen)

#tabla de frecuencia relativa para genero
nrow(dat)
ncol(dat)
table(dat$gen)/nrow(dat)

#min, max para una variable
min(dat$N1)
max(dat$N1)

#otra forma es con la funcion freq, paquete AMR
#este paquete funciona con versiones antiguas de R, de modo que, si no funciona en su sistema operativo
#pruebe con la alternativa siguiente:
#install.packages("rlang")
library(rlang)
#install.packages("bindrcpp")
library(bindrcpp)
#install.packages("AMR")
library(AMR)
freq(dat$gen,row.names=FALSE)
freq(dat$nhermanos,row.names=FALSE,sort.count=FALSE)


#paquete summarytools
#documentacion: https://www.rdocumentation.org/packages/summarytools/versions/0.6.5/topics/freq
install.packages("summarytools")
library(summarytools)
summarytools::freq(dat$nhermanos,round.digits=1,justify="l")

#grafico de barras en R
barplot(table(dat$conducta),main="Frecuencias por nivel de conducta",
        xlab="nivel de conducta",ylab="frecuencia absoluta",col="red")

#grafico circular en R
porc=round(table(dat$conducta)/nrow(dat)*100,1)
pie(table(dat$conducta),col=terrain.colors(nlevels(dat$conducta)),
    main="Composicion % por nivel de conducta",labels=porc)
legend("topright", c("Buena","Mala","Regular"),cex=0.6,fill=terrain.colors(nlevels(dat$conducta)))

#histograma en R
hist(dat$N1,col="darkseagreen3",
     main="Histograma notas prueba 1",ylab="frecuencia",xlab="notas",breaks="Sturges")

#poligono de frecuencia en R
g1=hist(dat$N1,col="darkseagreen3",
        main="Histograma notas prueba 1",ylab="frecuencia",xlab="notas",breaks="Sturges")
names(g1)
g1$mids
g1$counts
lines(g1$mids,g1$counts,type="b",col="red")