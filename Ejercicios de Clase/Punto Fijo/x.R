#1e-9 = 0.000000001
puntoFijo = function(g,x0,tol,maxIter){
  k = 1
  repeat{
    x1 = g(x0)
    dx = abs(x1-x0)
    x0 = x1
    cat("x ",k,"= ",x1,"\n")
    k =  k+1
    if(dx<tol || k > maxIter) break;
  }
  if( dx > tol ){
    cat("No hubo convergencia ")
    #return(NULL)
  } else{
    cat("x* es aproximadamente ", x1, " con  ", k-1," iteraciones\n")
  }
}
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
g <- function(x) exp(x)/pi
z <- function(x) x
y <- function(x) exp(x)-pi*x
curve(exp(x)/pi,0,2, col="blue");abline(h=-0,v=0,lty=6)
curve(z,0,2,col="red",add=TRUE)
curve(y,0,2,col="green",add=TRUE)
puntoFijo(g,0.5,1e-9,100)
biseccion(y,0,2,1e-9,100)
