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
    cat("x* es aproximadamente ", x1, " con  ", k," iteraciones")
  }
}
g <- function(x) log(x*pi)
curve(exp(x)-pi*x,0,4, col="blue");abline(h=0,v=0,lty=3)
curve(log(x*pi),0,4,col="red",add=TRUE)
puntoFijo(g,2.5,1e-9,100)
