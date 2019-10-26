


#three point endopoint formula 
#fuente: lectura derivacion R
#y https://www.math.ust.hk/~mamu/courses/231/Slides/CH04_1B.pdf para la utilizacion

h=0.1
x0 = 1.8
f <- function(x) x*cos(x)
derivadaAprox <- (1/2*h)*(-3*f(x0)+4*f(x0+h)-f(x0+2*h)) 