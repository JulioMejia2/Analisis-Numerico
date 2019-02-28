#a

be<-0
al<-3

A<-matrix(c(2, 0, 1,
             be,2 , -1,
             -1, 1, al), nrow=3, byrow=TRUE)
B<-matrix (c(1,2,1),nrow=3, byrow=TRUE)
Ab<-cbind(A,B)

print(Ab)

x<-0
y<-0
z<-0

diago1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

jaco<- function(A,b, x0, tol){
  x_k<-matrix(x0)
  
  D<-diago1(A)
  L<-tril(A,k=-1,diag = FALSE)
  U<-triu(A,k=1,diag = FALSE)
  
  it<-1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Err",it," ",norm(xk1-x_k,"F")/norm(x_k),"   ")
    x_k = xk1
    
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("it",it," ","x ",x[[it]]," ","y ",y[[it]]," ","z ",z[[it]],"\n")
    it = it + 1
    
    if(it == tol)
      break
  }
  cat("Sol", tol ," it ",x_k,"\n")
}

x1 = c(1,2,3)
jaco(A, B, x1, 10)

