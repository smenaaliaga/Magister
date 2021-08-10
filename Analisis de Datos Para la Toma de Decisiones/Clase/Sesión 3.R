#-----------------------------------
#sabado 21 de marzo
#-----------------------------------

#-----------------------------------
# funciones asociadas a la distr normal 
#-----------------------------------
# Normal Estandar
# distribucion acumulada
p1=pnorm(-0.9,mean=0,sd=1,lower.tail=TRUE) #estandarizada ya, pero no es necesario 
p1
p2=pnorm(+0.9,mean=0,sd=1,lower.tail=TRUE)
p2
p2-p1

# p3 es un vector numerico 
# PARA SIMULACIONES !!
p3 = rnorm(100,mean=1,sd=0.3)


# cuantiles
p4 = qnorm(0.1841,mean=0,sd=1,lower.tail = TRUE)
p4

z1=qnorm(0.025,mean=0,sd=1)
(z1/(-0.3))^2

# ------------
# Chi cuadrado
# acumulada
p1=pchisq(4.912,19)
p1

# cuantil
z1=qchisq(0.95,19,lower.tail=TRUE)
z1
