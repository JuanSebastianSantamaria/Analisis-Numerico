library(pracma)
library(Matrix)

#---------------------Punto 2 extra ----------------------

#Punto 2
A = matrix(c(8, 9, 2, 2, 7, 2, 2, 8, 6), nrow=3, byrow=TRUE)
b = c(69, 47, 68)
print("A")
print(A)

condicional = (cond(A))
print("Numero de condicion")
print(condicional)
print("b")
print(b)
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

library(BB)
library(psych)
A = matrix(c(8, 9, 2, 2, 7, 2,
             2, 8, 6), nrow=3, byrow=TRUE)

b= matrix(c(69,47,68), nrow=3, byrow=TRUE)
Ab = cbind(A,b)
Ab

#Matriz de Transicion Jacobi 
#L es una matriz triangular inferior con la diagonal en 0
L=A
L[lower.tri(L,diag=TRUE)] <- 0
#U es una matriz triangular superior con la diagonal en 0
U  = A
U[lower.tri(U,diag=TRUE)] <- 0
#S es una matriz triangular superior
S  = A
S[lower.tri(S,diag=FALSE)] <- 0
InvDiag <- solve(diag(diag(Ab)))
TJacobi = (-InvDiag)%*%(L+U)
TJacobi 
#Matriz de Transicion de Gauss Sidel
I=diag(1,nrow = nrow(A)) 
TGauss = -solve(I+InvDiag%*%L)%*%(InvDiag%*%S)
TGauss
#Matriz de Transicion de Relajacion
omegaM=0.5
Trelajacion= solve(I+omegaM*solve(InvDiag))%*%(I-omegaM*solve(InvDiag)*S)
Trelajacion
