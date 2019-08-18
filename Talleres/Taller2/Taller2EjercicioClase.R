library(pracma)
library(Matrix)

#---------------------Punto 1 extra ----------------------

#Punto 1 
n = 6
rrr =sample(0:20, 36, replace = TRUE)
A = matrix( rrr, nrow=6, byrow=TRUE)
condicional = 0
while(condicional< 1000 )
{
  A = matrix( sample(10:20, 36, replace = TRUE), nrow=n, byrow=TRUE)
  condicional = cond(A)
}
print("A")
print(A)

condicional = (cond(A))
print(condicional)
b = matrix( c(1,2,3,4,5,6), nrow = 1, byrow=TRUE)
print("b")
print(b)

if(condicional > 1000)
{
  print(condicional)  
  diagonal <- function(M) 
  {
    M[col(M)!=row(M)] <- 0
    return(M)
  }
  
  #T = -D^-1(L + U)
  D = diagonal(A)
  L = tril(A,k=-1)
  U = triu(A,k=1)
  
  T = (-solve(D))%*%(L+U)
  print("Matriz de transición")
  print(T)
  print("Norma")
  norma <- norm(T,"F")
  print(norma)
  print("Radio espectral de la matriz")
  
  radioExp <- max(abs(eig(A)))
  print( radioExp)
  
  # Matriz diagonal de dimension 3
  I=diag(1,nrow = nrow(A))
  # Matriz inversa de A
  D1 <- solve(D,I)
  T1 = D1 %*% U
  T2 = (I + (L %*% D1))
  # Matriz inversa de A
  T2<- solve(T2,I)
  
  #Analisi de convergencia
  MatTG = T1+T2
  normaG = norm(MatTG, type = c( "I"))
  print("Norma/convergencia de Gauss")
  print(normaG)
  print("Matriz de trancision de Gauss")
  print(MatTG)
  
  MatTJ = (-D1)%*%(L+U)
  normaJ = norm(MatTJ, type = c("I"))
  print("Norma/convergencia de Jacobi")
  print(normaJ)
  print("Matriz de trancision de Jacobi")
  print(MatTJ)
}
