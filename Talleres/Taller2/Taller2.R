#Taller 2 Análisis Numérico

library(pracma)
library(Matrix)

#Punto 1
#a)
#Revise las siguientes funciones con la matriz del ejercicio 2
n=4

D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)

print("D1")
print(D1)
print("D2")
print(D2)
print("D3")
print(D3)


A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=n, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)
print("b")
print(b)

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

#b)
#Evalue la matriz de transicion para el metodo SOR
#T = -D^-1(L + U)
D = diag1(A)
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)

T = (-solve(D))%*%(L+U)
print("T")
print(T)
print("Norma")
print(norm(T,"F"))


#Punto 2
#Dada la siguiente matriz, utilice las funciones del paquete
#para descomponer la matriz A=L+D+U (Jacobi)
#b)
#Utilice la funcion itersolve(A, b, tol , method = "Gauss-Seidel")
#y solucionar el sistema asociado a la matriz A con b=[1.45,3,5.12,-4]
#con una tolerancia de $1e^-9$
print("Gauss-Seidel:")
tol = 1e-9
#posible x0 -> x0=c(1,2,1,1)
sol = itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")
print(sol)

#c)
#Genere 5 iteraciones del metodo de Jacobi, calcular error
#relativo para cada iteracion
jacobiPr <- function(A,b, x0, tol){
  x_k = matrix(x0)
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == tol)
      break
  }
  cat("Solución a 5 iteraciones: ",x_k,"\n")
}

x0 = c(1,2,1,1)
jacobiPr(A, b, x0, 5)

#Punto 3 sistema AX=b
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)
#a)
#Implemente una funcion en R para que evalue las raices del polinomio
#caracteristico asociado a la matriz A
poli = charpoly(A, info = FALSE)

#b)
#Use el teorema de convergencia para determinar cual metodo iterativo
#es mas favorable.
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)
L[lower.tri(L,diag=TRUE)] <- 0
U[upper.tri(U, diag = TRUE)] <- 0
D = diag(diag(A))
I=diag(1,nrow = nrow(A)) # Matriz diagonal de dimension 3
D1 <- solve(D,I) # Matriz inversa de A
T1 = D1 %*% U
T2 = (I + (L %*% D1))
T2<- solve(T2,I) # Matriz inversa de A
MatTG = T1+T2
normaG = norm(MatTG, type = c( "I"))
print("Convergencia Gauss")
print(normaG)
MatTJ = (-D1)%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
print("Convergencia Jacobi")
print(normaJ)

#c)
#Evalue la matriz de transicion para cada caso
print("Matriz transicion Gauss")
print(MatTG)
print("Matriz transicion Jacobi")
print (MatTJ)

#d)
#Teniendo en cuenta lo anterior resolver el sistema
A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
A
b = c(1, 5, 1.5,-2.33)
b

X <- itersolve(A, b, method = "Jacobi")
print(X)
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
print(X)
#Solución por defecto
solucion<- solve(A,b)
print(solucion)


#Punto 3
#a)
#Pruebe el siguiente algoritmo con una matriz A_3
#modifiquelo para que a_{ii}=0 para todo i
tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
    M[col(M)==row(M)] <- 0
    
  }
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(tril1(M, k=1))

#b)
#Implemente una funcion en R para que dada una matriz A
#se obtenga una matriz diagonal D donde en la diagonal estan
#los mismos elementos de A
diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(diag1(M))

#Punto 4
#Cree una funcion que cuente el numero de multiplicaciones 
#en el metodo directo de Gauss Jordan, para resolver un sistema 
#de n ecuaciones y pruebelo para n=5

numeroMultiplicaciones = function(A, b){
  if(det(A)!=0){
    mult = 0
    n = nrow(A) # = ncol(A) para que sea cuadrada
    
    # matriz ampliada
    Ab = cbind(A,b)
    print(Ab)
    # Eliminación
    for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
      if(Ab[k,k]==0){ # intercambio de fila
        fila = which(Ab[k, ]!=0)[1]
        Ab[c(k, fila), ] = Ab[c(fila, k), ]
      }
      
      # Eliminación columna k
      for (i in (k+1):n){# debajo de la diagonal
        # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
        Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
        mult = mult + 2*(ncol(Ab))
      }
    }
    
    # Sustitución hacia atrás-------------------------
    # b(i) = A[i, n+1]
    x = rep(NA, times=n)
    x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
    mult = mult + n+1
    
    for(i in (n-1):1 ){
      x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
      mult = mult + 2*(n-2)
    }
    #cat(x, "\n")
    cat("Numero de multiplicaciones:", mult, "\n")
    return(x)
  } else {
    print("El determinante es 0, por lo tanto la matriz no tiene inversa")
  }
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)
cat("Punto 4: ","\n")
numeroMultiplicaciones(A,b)

#Punto 5

#a)
#Se llega a los valores de alpha y beta por las operaciones de +
# alpha > 1+1 
# beta + 1 < 2
# de acuerdo a su posiscion en  la matrix
beta = 0
alpha = 3

A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)
Ab = cbind(A,B)

print(Ab)

#b)
#Genere una tabla que tenga 10 iteraciones del metodo de Jacobi con vector
#inicial x_0=[1,2,3]
#c)
#Grafique cada ecuacion y la solucion
library("plot3D")

x = 0
y = 0
z = 0

diag1 <- function(M) {
  M[col(M)!=row(M)] <- 0
  return(M)
}

jacobiPr2 <- function(A,b, x0, tol){
  x_k = matrix(x0)
  D = diag1(A)
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  it = 1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("Solucion iteracion ",it,": ",x[[it]]," ",y[[it]]," ",z[[it]],"\n")
    it = it + 1
    if(it == tol)
      break
  }
  lines3D(x, y, z, colvar = z, col = NULL, add = FALSE, theta = 20, phi = 20)
  cat("Solucion a ", tol ," iteraciones: ",x_k,"\n")
}

x1 = c(1,2,3)
jacobiPr2(A, B, x1, 10)

#Punto 6
#Instalar el paquete Matrix y descomponga la matriz A
#(del punto dos) de la forma LU y factorizarla como A=QR
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
print("b")
print(b)

Ab = cbind(A,b)
print(Ab)

#matrices diagonales
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)
print(L)
print(U)

#factorizacion QR
gs <- gramSchmidt(A)
(Q <- gs$Q); (R <- gs$R)
print(Q)
print(R)
print(Q %*% R)  # = A

#Punto 7
#a)
#Determinar numericamente la interseccion entre la circunferencia x^2+y^2 = 1
#y la recta y = x. Usamos una aproximacion inicial (1,1). Utilice el 
#paquete BB y  la funcion BBsolve() del paquete, grafique la solucion
library(BB)
ecuaciones = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) # n initial starting guess
sol = BBsolve(par=p0, fn=ecuaciones)
sol$par

plot(sol$par)
plot(ecuaciones)

#b)
#Analizar y comentar el siguiente codigo
trigexp = function(x) {
  
  #Tamaño del vector que llega por parámetro
  n = length(x)
  #se crea un vector F vacío
  F = rep(NA, n)
  #Se enuncian las ecuaciones del sistema
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #Se crea una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se evalúan tn1 ecuaciones
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se evalúa la última ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  #Se retorna F
  F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
#se halla la solcuión del sistema trigexp usando BBsolve de la librería BB, utilizando n valores iniciales
sol = BBsolve(par=p0, fn=trigexp)
#Muestra el vector solución del sistema para cada n valores iniciales
sol$par

#Punto 8
#Demuestre y realice varias pruebas que la matriz de transicion por el
#metodo de Gauss-Seidel esta dada por T=(-D^{-1}U)(I+LD^{-1})^{-1}
N <- 3
A <- Diag(rep(3,N)) + Diag(rep(-2, N-1), k=-1) + Diag(rep(-1, N-1), k=1)
x0 <- rep(0, N)
b = c(4,5,6)

itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")