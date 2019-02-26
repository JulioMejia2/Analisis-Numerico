A = matrix(c(-8.1,-7.00,6.123,-2.0,
             -1.0,4.00,-3.000,-1.0,
             0.0,-1.00,-5.000,0.6,
             -1.0,0.33,6.000,0.5), nrow=4, byrow=TRUE)
A
B = c(1.45,3,5.12,14)
B
transp = t(B)
transp
itersolve(A,transp,tol=1^-9,method=c("Gauss-Seidel"))
itersolve(A,transp,nmax = 5,tol=1^-9,method=c("Gauss-Seidel"))
