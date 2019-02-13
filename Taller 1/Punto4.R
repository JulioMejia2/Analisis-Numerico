biseccion = function(f,a,b,tol,maxIter){
  k = 1
  repeat{
    c = (a+b)/2
    if(abs(f(c))<tol) {
      break
    }
    
    if(f(a)*f(c)<0) b = c
    
    if(f(b)*f(c)<0) a = c
    
    if(k>maxIter) break;
    
    cat("x ",k,"= ",c,"\n")
    k = k+1
  }
  cat("\n Con biseccion \n La raiz es aproximadamente ", c, " con  ", k-1," iteraciones")
  
}
newtonDN = function(f, x0, tol, maxiter){
  # Derivada numérica con diferencia central
  fp = function(x) { h = 1e-4
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
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  angulo <- seq(-max(theta),max(theta),by=theta[2]-theta[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
}
g <- function(x) 2+cos(3*x)-2+exp(x)
curve(g,-2,2, col="blue");abline(h=-0,v=0,lty=6)
biseccion(g,-pi/2,0,1e-4,100)

dim <- seq(-pi, pi, by=pi/300) 
r=2+cos(3*dim)
r1 = 2-exp(dim)
polar(dim,r,"blue")
par(new="TRUE")
polar(dim,r1,"red")
newtonDN(g,-pi/2,1e-4,10)

