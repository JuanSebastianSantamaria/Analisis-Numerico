f <- function(x) (x*exp(x))
r <- 5.436563656918091
h <- 0.1
aux <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
x <- c()
eAux <- c()
for (i in aux) {
  d <- (f(1+h)-f(1))/h
  e <- abs(r-d)
  cat(h,r,d,e, "\n")
  eAux <- c(eAux, e)
  x <- c(x, h)
  h <- h/10
}
# precision = (valor-error)/valor
y <- c()
for (i in eAux) {
  precision = abs((r-i)/r)
  y <- c(y, precision)
}
length(x)
length(y)
plot(x, y, xlim=c(0,0.1), ylim=c(0, 1), main = "Precisión vs Tamaño de paso", xlab = "Tamaño de paso", ylab = "Precisión")
lines(x, y, col = "blue")