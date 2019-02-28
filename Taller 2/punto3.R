#a
tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
    M[col(M)==row(M)] <- 0
    
  }
  return(M)
}

M = magic(3)
print(M)
print(tril1(M, k=1))

#b
diago1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

M = magic(3)
print(M)
print(diago1(M))

