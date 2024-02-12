

sur <- function(m.m, m.r) {
  1/(1 + exp(-alpha*(m.m + m.r - 1)))
}

alpha <- 15
curve(sur(x, m.r = 0.5), from = 0, to = 1, ylim = c(0,1), col = "blue",
      xlab = expression("mutant gamete size, m'"),
      ylab = expression("zygote survival, s"))

w <- function(m.m, m.r) {
  (M/m.m) * sur(m.m, m.r)
}

s(0.7,0.4)

s <- function(m.m, m.r) {
  w(m.m, m.r) - w(m.r, m.r)
}

M <- 1

PIP <- outer(seq(0, 1, 0.005), seq(0, 1, 0.005), s)
# the computes s(h', h) for all combinations of h' and h

par(pty="s")
filled.contour(t(PIP), levels = c(-1, 0, 1), main = "PIP", las = 2,
               xlab = expression("resident gamete size m"),
               ylab = expression("mutant gamete size m'"))
