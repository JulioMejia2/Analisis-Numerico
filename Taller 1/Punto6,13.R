################################################################################
newtonraizentero=function(n,ex,x0,maxi,tol){
  fs=0
  i=1
  repeat{
    ecu=((x0^ex)-n)/(ex*(x0^(ex-1)))
    f=x0-ecu
    error=abs(fs-f)
    if(error<tol) break;
    cat("iter= ",i,"\t","f= ",f,"\t","error= ",error,"\n")
    fs=f
    x0=f
    i=i+1
    if(i>maxi)break;
      
  }
  cat("raiz= ",f,"error= ",error)
}

newtonraizentero(550605,2,100,50,1e-8)

