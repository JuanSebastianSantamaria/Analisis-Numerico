library(pracma)
library(Matrix)


A = matrix(c(8, 9, 2, 2, 7, 2, 2, 8, 6), nrow=3, byrow=TRUE)
b = c(69, 47, 68)
print(A)
print(1/rcond(A))

#Funcion para la diagonal
diag1 <- function(M) {
  M[col(M)!=row(M)] <- 0
  return(M)
}

D = diag1(A)
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)
T = (-solve(D))%*%(L+U)
radioEspectral = max(abs(eig(A)))
print("T")
print(T)
print("Norma")
print(norm(T,"F"))
print("Radio espectral")
print(radioEspectral)