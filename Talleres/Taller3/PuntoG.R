
# g. Aplique la fórmula adecuada para aproximar 𝑓"(1.8) justifique su respuesta


h=0.1
x0 = 1.8
f <- function(x) x*cos(x)

segundaDerivadaAprox <- ( (1/(h^2) ) * ( (f(x0+h)) - (2*f(x0)) + (f(x0-h)) ) ) 

segundaDerivadaAprox

#Justificación:
#   Lo anteriormente realizado consiste en en un ajuste de un
#   polinomio en el punto de interés. La aproximación centrada
#   estándar de la segunda derivada está definida por el 
#   polinomio descrito. 
#   Se sabe bien que esta polinomio es una derivación de la 
#   fórmula de los tres puntos progresiva. Progresiva hace
#   referencia a que se inicia desde un h base y se va
#   aumentando el valor de h, a diferencia de centradas o 
#   regresivas que toman en cuenta valores anteriores. Se 7
#   obtó por elegir una progresiva pues a partir de pruebas
#   fue la fórmula que proveyó un mejor resultado.
