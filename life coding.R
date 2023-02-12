f <- function(h) {
  1 - h^alpha
}

alpha <- 2
curve(f(x), from = 0, to = 1, col ="blue", xlab = "plant height, h", ylab = "proportion of leaf tissue, f(h)")
alpha <- 3
curve(f(x), from = 0, to = 1, col ="orange", add = TRUE)
alpha <- 5
curve(f(x), from = 0, to = 1, col ="green", add = TRUE)
legend("bottomleft", legend = c(expression(paste(alpha, " = ", 2)),
                                expression(paste(alpha, " = ", 3)),
                                expression(paste(alpha, " = ", 5))),
       lty =1, col = c("blue", "orange", "green"))