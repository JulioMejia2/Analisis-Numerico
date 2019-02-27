tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = FALSE)] <- 0
    for(i in 1:3 ){
      for(j in 1:3){
        if(i==j){
          M[i,j]=0   
        }
      }
    }
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
    for(i in 1:3 ){
      for(j in 1:3){
        if(i==j){
          M[i,j]=0   
        }
      }
    }
  }
  return(M)
}
diagonal<-function(M){
  M<-diag(diag(M))
  return(M)
  
}
A <- matrix(c(-6,10,-8,
              8,0,-5,
              -6,-9,-2),nrow = 3,byrow = FALSE)
A
#Punto 4a
tril1(A,k=0)
#Punto 4b
diagonal(A)
