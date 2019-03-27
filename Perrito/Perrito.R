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
indiceDeJaccard<-function(g1,g2,tol,maxIter){
  aciertos<-0
  i<-1
  repeat{
    dx = abs(g1(i)-g2(i))
    if(dx<=tol){
      aciertos = aciertos +1
    }
    if (i == maxIter){
      break;
    }
    i = i+0.5
  }
  cat("Número de aciertos: ", aciertos,"\n")
}
#Spline cúbico teórico 
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)     
s1<-splinefun(x,y,method="monoH.FC",ties=mean)

#Spline cúbico parte de arriba perrito
x1 = c(1.716,2.795,6.475,8.977,10.695,12.853,17.343,20.581,22.445,24.310,25.954,27.597,27.990,29.265,29.952)
y1 = c(3.278,4.186,4.456,7.081,7.719,7.498,5.069,7.817,7.253,6.173,6.296,5.560,4.873,4.480,3.622)
plot(x1,y1, pch=20, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Perrito")
s2<-splinefun(x1,y1,method="monoH.FC",ties=mean)
curve(s2(x),add = TRUE, col = 2, n = 1001)
#Comparación del modelo propuesto versus el teórico
curve(s1(x),add = TRUE, col = 3, n = 1001)
#Máximo error permitido 0.3!
#Hacer función que calcule esto y cuente el numero de aciertos sobre todo el rango!
#Spline cúbico parte de abajo perrito
x2 = c(1.716,3.384,7.579,8.928,9.026,11.381,14,14.865,29.952,16.509,17.735,18.336,18.962,21.219,22.912,25.070,27.303,27.892,28.971,29.373)
y2 = c(3.278,3.131,2.984,3.597,3.229,2.322,2,2.150,3.622,2.150,2.076,1.812,2.076,1.856,1.586,1.757,2.322,2.910,2.739,3.047)
par(new = TRUE)
#plot(x2,y2, pch=20, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Perrito")
s3<-splinefun(x2,y2,method="monoH.FC",ties=mean)
curve(s3(x),add = TRUE, col = 2, n = 1001)
indiceDeJaccard(s1,s2,0.5,30)
