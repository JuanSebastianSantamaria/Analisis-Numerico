
# e. Realice una modificación de la fórmula de los tres puntos, tomando valores entre
#  (𝑥0 − ℎ) y (𝑥0 + ℎ) y compare la magnitud del error con la fórmula de la parte e.

h=0.1
x0 = 1.8
f <- function(x) x*cos(x)

derivadaAprox <- ( (1/(2*h) ) * ( f(x0+h) - f(x0-h) ) ) 
derivadaAprox

derivadaAproxD <- -0.01989079 *100
valorReal <- (-1.980127)

errorD <- (abs((valorReal-derivadaAproxD)/valorReal)*100)
error <- (abs((valorReal-derivadaAprox)/valorReal)*100)

error
errorD

cat("Valor real    -> ",valorReal,
    "\nValor punto D -> ", derivadaAproxD, " | error -> ", errorD,
    "\nValor punto e -> ", derivadaAprox, " | error -> ", error)