itersolveModificado <- function (A, b, x0 = NULL, nmax = 1000, tol = .Machine$double.eps^(0.5), 
          method = c("Gauss-Seidel", "Jacobi", "Richardson")) 
{
  stopifnot(is.numeric(A), is.numeric(b))
  n <- nrow(A)
  if (ncol(A) != n) 
    stop("Argument 'A' must be a square, positive definite matrix.")
  b <- c(b)
  if (length(b) != n) 
    stop("Argument 'b' must have the length 'n = ncol(A) = nrow(A).")
  if (is.null(x0)) {
    x0 <- rep(0, n)
  }
  else {
    stopifnot(is.numeric(x0))
    x0 <- c(x0)
    if (length(x0) != n) 
      stop("Argument 'x0' must have the length 'n=ncol(A)=nrow(A).")
  }
  method <- match.arg(method)
  if (method == "Jacobi") {
    L <- diag(diag(A))
    U <- eye(n)
    beta <- 1
    alpha <- 1
  }
  else if (method == "Gauss-Seidel") {
    L <- tril(A)
    U <- eye(n)
    beta <- 1
    alpha <- 1
  }
  else {
    L <- eye(n)
    U <- L
    beta <- 0
  }
  b <- as.matrix(b)
  x <- x0 <- as.matrix(x0)
  r <- b - A %*% x0
  r0 <- err <- norm(r, "f")
  iter <- 0
  while (err > tol && iter < nmax) {
    iter <- iter + 1
    z <- qr.solve(L, r)
    z <- qr.solve(U, z)
    if (beta == 0) 
      alpha <- drop(t(z) %*% r/(t(z) %*% A %*% z))
    x <- x + alpha * z
    r <- b - A %*% x
    err <- norm(r, "f")/r0
    cat("Iteracion ",iter, "error relativo= ",err,"\n")
  }
  cat ("\nNumero de iteraciones realizado fue ",iter)
  
  cat ("\n\nSoluciones:")
  print(c(x))
}

##Punto 2a ##
require(Matrix)
require(pracma)
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
A
B <- matrix(c(1.45,3,5.12,4.0), nrow = 4, ncol = 1, byrow = TRUE)
B
#Matriz diagonal inferior
ludec = lu(A)
ludec
L <- ludec$L
L
#Matriz triangular superior
U <-ludec$U
U
#Diagonal de la matriz
D <- diag(diag(A))
D
#Matriz orginal
A = L %*% U
A
##Punto 2b ##
itersolve(A, B, tol = 1e-9, method = "Gauss-Seidel")
##Punto 2c ##
itersolveModificado(A, b, nmax = 5, tol = 1e-9, method = "Jacobi")
