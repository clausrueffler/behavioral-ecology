f <- function(h) {
  1 - h^alpha
}

g <- function(h.1, h.2) {
  P.L + (P.H - P.L)/(1 + exp(-beta*(h.1 - h.2)))
}

alpha <- 3
beta <- 10
P.L <- 0.25
P.H <- 1

w <- function(h.m, h.r) {
  f(h.m) * g(h.m, h.r)
}

s <- function(h.m, h.r) {
  w(h.m, h.r) - w(h.r, h.r)
}

PIP <- outer(seq(0, 1, 0.005), seq(0, 1, 0.005), s)
# the computes s(h', h) for all combinations of h' and h

par(pty="s")
filled.contour(t(PIP), levels = c(-1, 0, 1), main = "PIP", las = 2,
               xlab = expression("resident height h"),
               ylab = expression("mutant height h'"))
