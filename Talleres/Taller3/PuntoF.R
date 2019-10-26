# f. Utilice la fórmula para cinco puntos alrededor de 𝑥0 y aplíquela y compárela con
# todas las formulas anteriores

h=0.1
x0 = 1.8
f <- function(x) x*cos(x)
derivadaAprox <- ( (1/12*h)*( f(x0-(2*h)) - (8*f(x0-h)) + (8*f(x0+h)) - (f(x0+(2*h) )) ) )

#Este código permite obtener la derivada utilizando 5 puntos desde h*0 hasta h*4
#derivadaAprox <- ( (1/12*h)*( (-25*f(x0)) + (48*f(x0+h)) - 
#(36*f(x0+(2*h))) + (16*f(x0+(3*h))) - (3*f(x0+(4*h))) ) )

derivadaAprox5 <- derivadaAprox *100
derivadaAprox3 <- -0.01989079 *100
valorPuntoA <- (-1.980205)
valorReal <- (-1.980127)

derivadaAprox3
derivadaAprox5
valorReal

error3 <- (abs((valorReal-derivadaAprox3)/valorReal)*100)
error5 <- (abs((valorReal-derivadaAprox5)/valorReal)*100)
errorA <- (abs((valorReal-valorPuntoA)/valorReal)*100)

error3
error5


cat("Valor Obtenido con tres puntos               -> ",derivadaAprox3, 
"\nValor obtenido con 5 puntos al rededor de x0 -> ", derivadaAprox5,
"\n Valor obtenido en el punto A                -> ", valorPuntoA,
"\nValor Real                                   ->  -1.980127\n\n",
"Error con tres puntos  -> ", error3,
"\n Error con cinco puntos -> ", error5,
"\n Erroe del punto A      -> ", errorA)


