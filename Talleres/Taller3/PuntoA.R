


#three point endopoint formula 
#fuente: lectura derivacion R
#y https://www.math.ust.hk/~mamu/courses/231/Slides/CH04_1B.pdf para la utilizacion

x0 = 1.8
f <- function(x) x*cos(x)
h=0.1
derivadaAprox <- (f(x0+h)-f(x0))/h

resultados <- c(derivadaAprox)
hs <- c("0.1")

h=0.01
derivadaAprox <- (f(x0+h)-f(x0))/h
resultados <- c(resultados, derivadaAprox)
hs <- c(hs,"0.01")
h=0.0011
derivadaAprox <- (f(x0+h)-f(x0))/h
resultados <- c(resultados, derivadaAprox)
hs <- c(hs,"0.0011")
h=0.0001
derivadaAprox <- (f(x0+h)-f(x0))/h
resultados <- c(resultados, derivadaAprox)


tabla <-matrix(resultados, nrow = 4, byrow=TRUE)
rownames(tabla) <- c("h=0.1","h=0.01","h=0.0011","h=0.0001")
colnames(tabla) <- c("f(1.8) Aproximacion")
as.table(tabla)