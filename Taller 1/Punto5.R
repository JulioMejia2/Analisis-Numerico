m = 25
n = 0.4
r = n * (10**4)
n = r * (10**-4)
r = n
digitos = 0
while (r != 0){
  r =  r / 10
  digitos = digitos+1
}
errorredondeo = 1 * (10**(digitos-m))
errorrelativo = 1 * (10**(1-m))
cat("Número: ", n ,"\nError de redondeo: ", eredon, "\nError relativo: ", erela)
