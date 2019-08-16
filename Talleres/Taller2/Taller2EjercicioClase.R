library(pracma)
library(Matrix)

while((1/rcond(A)) < 1000){
  A = matrix(sample(10:15,36,replace=T), nrow=6, byrow=TRUE)
  b = c(1, 2, 3, 4, 5, 6)
}
print(A)
print(1/rcond(A))

#Funcion para la diagonal
diag1 <- function(M) {
  M[col(M)!=row(M)] <- 0
  return(M)
}

if((1/rcond(A)) > 1000){
  #Matriz transicion metodo de Jacobi
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  L[lower.tri(L,diag=TRUE)] <- 0
  U[upper.tri(U, diag = TRUE)] <- 0
  D = diag(diag(A))
  I=diag(1,nrow = nrow(A)) # Matriz diagonal de dimension 6
  D1 <- solve(D,I) # Matriz inversa de A
  T1 = D1 %*% U
  T2 = (I + (L %*% D1))
  T2<- solve(T2,I) # Matriz inversa de A
  MatTG = T1+T2
  normaG = norm(MatTG, type = c( "I"))
  print("Matriz de transicion de Gauss")
  print(MatTG)
  print("Norma de la matriz por Gauss")
  print(normaG)
  print("Convergencia Gauss")
  print(normaG)
  MatTJ = (-D1)%*%(L+U)
  normaJ = norm(MatTJ, type = c("I"))
  print("Matriz de transicion de Jacobi")
  print(MatTJ)
  print("Norma de la matriz por Jacobi")
  print(normaJ)
  print("Convergencia Jacobi")
  print(normaJ)
}
