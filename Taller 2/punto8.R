library(BB)
ec = function(x) {
  n = length(x)
  #se crea un vector F vacío
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) # solución inicial
sol = BBsolve(par=p0, fn=ecuaciones)
sol$par

plot(sol$par)
plot(ecuaciones)

#b
trigexp = function(x) {
  
  #Tamaño del vector 
  n = length(x)
  
  #se crea un vector F vacío
  F = rep(NA, n)
  
  #Se enuncian las ecuaciones del sistema
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #Se crea una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se evalúan tn1 ecuaciones
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se evalúa la última ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  #Se retorna F
  F
}
#Número de iteraciones
n = 10000
p0 = runif(n) # n initial random starting guesses
#se halla la solcuión del sistema trigexp usando BBsolve de la librería BB, utilizando n valores iniciales aleatorios
sol = BBsolve(par=p0, fn=trigexp)
#Muestra el vector solución del sistema para cada n valores iniciales
sol$par
