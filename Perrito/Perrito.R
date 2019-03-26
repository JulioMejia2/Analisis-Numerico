library(Matrix)
library(PolynomF)
require(pracma)
Graficar<-function(x0, xn){
  xi = x[x0:xn]
  yi = y[x0:xn]
  x <- seq(x[x0], x[xn], len=20)
  y <- barylag(xi, yi, x)
  lines(x, y, col="brown")
}

x=c(1,2  ,6  ,8.1 ,13 ,17.6,20,26.5,29,30) 
y=c(3,3.7,4.5,6.7 ,6.7,4.5 ,7 ,5.15  ,4.1,3) 

plot(x,y, pch=20, cex=1, col = "black", asp=1,xlab="X", ylab="Y", main="Perrito")

Graficar (1,3)
Graficar(3,6)
Graficar(6,8)
Graficar(8,10)

DatosX = x[1:3]; DatosY = y[1:3]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio

DatosX = x[3:6]; DatosY = y[3:6]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio

DatosX = x[6:8]; DatosY = y[6:8]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio

DatosX = x[8:10]; DatosY = y[8:10]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio


