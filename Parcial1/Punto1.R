#Punto 1b
A <- matrix(c(4, -1, -1, -1,
             -1, 4, -1, -1,
             -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)

Modificada <- matrix(c(4, -1.15, -1, -1,
                      -1, 4, -1, -0.9,
                      -1, -1, 4, -1,
                      -1, -1, -1, 4), nrow=4, byrow=TRUE)

b = c(-exp(1),5,6,0)
#Se realiza la solución para la matriz original
sol1 <-solve(A,b)
cat("Solucion 1: \n")
sol1
#Se realiza la solución para la matriz modificada
cat("Solucion 2: \n")
sol2<- solve(Modificada,b)
sol2
cat("\n")
#Vector del error de la solución con la matriz modificada
errorSolucion = sol2-sol1
errorSolucion
#Norma infinita del error
nE = Norm(errorSolucion,Inf)
#Norma infinita de la solución con matriz modificada
nXbarra = Norm(sol2,Inf)
#Cálculo de la variación
variacion <- (nE/nXbarra)*100
variacion

#Cálculo de la cota
nA <- norm(A,"I")
cota <-(0.001/nA)*100
cota
