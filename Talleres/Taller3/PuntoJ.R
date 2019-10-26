E <- function(L,R,i,di,dt) (L*(di/dt)+R*i)
r <- 0.142
l <- 0.98
cat("Voltaje en 1.01: ",E(l, r, 3.12, 0.02, 0.01))
cat("Voltaje en 1.02: ",E(l, r, 3.14, 0.02, 0.01))
cat("Voltaje en 1.03: ",E(l, r, 3.18, 0.04, 0.01))
cat("Voltaje en 1.04: ",E(l, r, 3.24, 0.06, 0.01))