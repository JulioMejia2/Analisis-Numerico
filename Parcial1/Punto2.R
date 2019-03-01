Tx <- function(x)  tan(pi*x)
Gx <- function(x)  sin(pi*x)

#Función en la cual se desea determinar la raiz entre las funciones
Fx<- function(x) tan(pi*x)-sin(pi*x)
Dx <- function(x) (pi*(sec(pi*x))^2)-(pi*cos(pi*x))
#Funcion recursiva
recursivo<-function(x1,x2,tol)
{
  error<-tol+1
  x0<-x1-((Fx(x1)*(x1-x2))/(Fx(x1)-Fx(x2)))
  
  x1<-x2
  x2<-x0
  i<-0
  while(error>tol)
  {
    x3<-x1-((Fx(x1)*(x1-x2))/(Fx(x1)-Fx(x2)))
    error<-abs((x3-x2)/x3)*100
    cat("x=",x3,"\t Error:",error,"\t iteracion: ",i,"\n")
    x1<-x2
    x2<-x3
    i<-i+1
  }
  
}
#Función de Newton acelerado
newtonAitken<-function(r,tol){
  i<-0
  error<-1
  while(i<=3 && error>tol){
    if(r!=0){ 
      bef=r
      r<-r-((Fx(r))/Dx(r))
      error<-(abs(bef-r))/abs(bef)
      cat("R=",r,"\t Error:",error,"i:",i,"\n")
      if(i==1)
      {
        x0=r
      }
      else
      {
        if(i==2)
        {
          x1=r
        } 
        else
        {
          x2=r
        }
      }
    }
    i=i+1
  }
  
  while(error>tol){
    if(r!=0){
      x3=x2-(((x2-x1)^2)/(x2-(2*x1)+x0))
      x0=x1
      x1=x2
      error<-(abs(x2-x3))/abs(x2)
      cat("R=",x3,"\t Error:",error,"iteracion:",i,"\n")
      r<-r-((Fx(r))/Dx(r))
      x2=r
    }
    i=i+1
  }
}

curve(Fx,-2,2, col="blue");abline(h=-0,v=0,lty=6)

recursivo(0.9,1.3,1.e-9)
recursivo(1.9,2.2,1.e-9)

#Utilizando Aitken

newtonAitken(1.3,1.e-9)

