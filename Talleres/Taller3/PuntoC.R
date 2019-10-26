


#three point endopoint formula 
#fuente: lectura derivacion R
#y https://www.math.ust.hk/~mamu/courses/231/Slides/CH04_1B.pdf para la utilizacion


f <- function(x) x*cos(x)
f1 = expression(cos(x) - x * sin(x))
D(f1, 'x')
#segundaDerivada
f2 <- function(x) -(sin(x) + (sin(x) + x * cos(x)))
M = f2(1.8)

#diciendo que nuestro objetivo es 
# 10^-4 = error
# y la formula de nuestro error es abs((h*M)/2) entonces
cotaError <- function(h) abs((h*M)/2) -10^-4
respuesta <- uniroot(cotaError, c(0,1))$root
cat("h = ", respuesta, " nos da una precision de 10^-4 para la derivada")