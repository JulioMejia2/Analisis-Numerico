library(Matrix)
library(PolynomF)

x = c(6,8,10,12,14,16,18,20)
y = c(7,9,12,18,21,19,15,10)

#Interpolación con datos suministrados
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")

DatosX = x[1:8]; DatosY = y[1:8]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio
par(new= TRUE)
curve(Ajuste_Polinomio,add=T,from =0,to =25)

#Datos estimados
x1 = c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
DatosX1 = x1[1:15];
DatosX1
DatosY1 = Ajuste_Polinomio(DatosX1[1:15]);
DatosY1
Ajuste_Polinomio1 = poly.calc(DatosX1,DatosY1)
Ajuste_Polinomio
par(new= FALSE)
plot(DatosX1,DatosY1, pch=19, cex=1, col = "green", asp=1,xlab="X", ylab="Y", main="Diagrama ")
par(new= TRUE)
curve(Ajuste_Polinomio1,add=T,from =0,to =25)

#Error del modelo planteado
yInter = Ajuste_Polinomio1(DatosX[1:8])
yInter
for(i in 1:8){
  Error<-abs(y(i)-yInter(i)/y(i)
  cat("Error relativo ",i," : ",Error)
}
