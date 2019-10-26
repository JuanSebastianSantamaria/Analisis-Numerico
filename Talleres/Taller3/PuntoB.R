


#three point endopoint formula 
#fuente: lectura derivacion R
#y https://www.math.ust.hk/~mamu/courses/231/Slides/CH04_1B.pdf para la utilizacion

x0 = 1.8
f <- function(x) x*cos(x)
f1 = expression(cos(x) - x * sin(x))
D(f1, 'x')
#segundaDerivada
f2 <- function(x) -(sin(x) + (sin(x) + x * cos(x)))
res = f2(1.8) #res es lo mismo que M en el documento 
rest <- c(abs(0.1*res)/2,abs(0.01*res)/2,abs(0.0011*res)/2, abs(0.0001*res)/2)
rest