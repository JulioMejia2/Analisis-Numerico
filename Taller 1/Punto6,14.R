intuitivo = function(f,a,b,tol,maxIter){
  k = 0
  x = a
  d = (b-a)/10
  repeat{
    x = x + d
    if(f(x)<0){
      x = x-d
      d = d/10
    }
    
    if(k>maxIter) break;
    
    if(abs(d)<tol) {
      break
    }
    
    cat("x ","= ",x,"k= ",k," d= ",d,"\n")
    k = k+1
  }
  cat("\n La raiz es aproximadamente ", x, " con  ", k," iteraciones")
}
x=seq(0,2,by=0.1)
f = function(x) exp(x)-(pi*x)
intuitivo(f,0,1,1e-12,100)
plot(x,(exp(x)-pi*x),type="l",col="blue",lwd=3)
abline(h=0,v=0, lty=3)
