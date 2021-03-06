################################################################################
#Punto 3

newtonDN = function(f, x0, tol, maxiter){
  # Derivada num�rica con diferencia central
  x<-seq(x0-2,x0+2,0.001)
  plot(x,f(x),type="l",col="blue")
  fp = function(x) { h = 1e-15
  (f(x+h) - f(x-h)) / (2*h)
  }
  k = 0
  #Par imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\
      n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dx = abs(correccion)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanz� el m�ximo n�mero de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
## --- Pruebas
f = function(x) (3*sin(x)*cos(x))-(4*sin(x))+(cos(x))
options(digits = 15)
newtonDN(f, 0.5, 1e-4, 10)




