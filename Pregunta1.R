#Solucion Pregunta 1
#ensayo de Bernoulli

x<- c(0,1)
fx<- c(0.68,0.32)
#tabla 

cbind(x,fx)
plot(x,fx,ylim=c(0,1),type='h',col='red')
points(x,fx, pch=16, col='red')

n<- 400000
muestra<- sample(x,n,fx, replace=TRUE)
fi<- table(muestra)/n
fi
br<- barplot(fi) #frequencias relativas
lines(br,fx,ylim=c(0,1),type='h',col='red') #funcion de masa de probabilidad
points(br,fx,pch=16,col='red')

#datos
xbar<-mean(muestra)
xbar

#modelo
mu<-sum(x*fx) #valor esperado
mu

#datos varianza muestral
ssq<- var(muestra)
ssq

#varianza de la funcion de masa de probabilidad
sigmasq<- sum((x-mu)^2*fx)
sigmasq

#1
n<- 43
set.seed(123)
muestra<- sample(x,n,fx, replace=TRUE)
muestra

y<- function(i){sum(sample(x,n,fx, replace=TRUE))}
y(4)

#bucle en R
set.seed(123)
m<- 400000 #encuestas de n=43
encuestas<- sapply(1:m,y)
fi<-table(encuestas)/m
fi
data.frame(fi)

dbinom(13,43,0.32) #valor de la chincheta

#tabla de probabilidad
resultados<- 1:43
fy<- dbinom(resultados,43,0.32)

tabladeprobablidad<- cbind(resultados,fy)
tabladeprobablidad

#2
#n=44
resultados<- 1:44
fy<- dbinom(resultados,44,0.32)

tabladeprobablidad<- cbind(resultados,fy)
tabladeprobablidad

plot(resultados,fy,type='h',col='red',ylim=c(0,0.2))

Fy<- cumsum(fy)
tabladeprobablidad<- cbind(resultados,fy,Fy)
tabladeprobablidad
#respuesta es la acumuluacion hasta 17

plot(Fy,type='s',col='red')

pbinom(17,44,0.32)

#3
#n=24
resultados<- 1:24
fy<- dbinom(resultados,24,0.68)
Fy<- cumsum(fy)

tabladeprobablidad<- cbind(resultados,fy,Fy)
tabladeprobablidad

mu<- sum(resultados*fy)
mu

sigmasq<- sum((resultados-mu)^2*fy)
sigmasq

plot(Fy,type='s',col='red')

qbinom(0.25,24,0.68)

#4
