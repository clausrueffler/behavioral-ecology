f <- function(h) {
  1 - h^alpha
}

# fixed P.L

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

# variable P.L

P.L.flex <- function(h.m, h.r) {# P.L.flex is the name of the function that makes the minimum photosynthetic activity a function of the degree of shading of the leaves by the neighboring plant
  P.L.mean + P.L.var*(1 / (1 + exp(gamma*(h.m - h.r))) - 1/2)
}

g.flex <- function(h.m, h.r) { # this a new function for g(h', h) that incorporates our new function for P.L
  P.L.flex(h.m, h.r) + (P.H - P.L.flex(h.m, h.r))/(1 + exp(-beta*(h.m - h.r)))
}

beta <- 5 # how fast does g change
P.H <- 1 # maximum photosynthesis (identical in both versions of the model)
P.L.mean <- 0.25 # parameter P.L.mean gives P.L when h'= h
P.L.var <- 0.3 # parameter determining the maximum difference in P.L between completely shaded and maximally sun exposed
gamma <- 10 # parameter determining how fast P.L increases with increased shading from a competitor
 

curve(P.L.flex(x, h.r = 0.5), from = 0, to = 1, xlab = expression(height ~ "h'"), ylab = expression("minimum photosynthesis, " * P[L]("h'", h * "=0.5")))

curve(g.flex(x, h.r = 0.5), from = 0, to = 1, xlab = expression("plant height, h'"), ylim = c(0,1), ylab = expression("photosythesis/leaf, g(h', h = 0.5)"), col = "blue")

w.2 <- function(h.m, h.r) { # payoff function now based on flexible P.L
  f(h.m) * g.flex(h.m, h.r)
}

s <- function(h.m, h.r) { # invasion fitness
  w.2(h.m, h.r) - w.2(h.r, h.r)
}

PIP <- outer(seq(0, 1, 0.005), seq(0, 1, 0.005), s)
par(pty="s")
filled.contour(t(PIP), levels = c(-1, 0, 1), las = 2, main = "PIP",
               xlab = expression("height h"),
               ylab = expression("height h'"))

s.flipped.roles <- function(h.m, h.r) {
  w.2(h.r, h.m) - w.2(h.m, h.m)
}

PIP.mirrored <- outer(seq(0, 1, 0.005), seq(0, 1, 0.005), s.flipped.roles)

par(pty="s")
filled.contour(t(PIP.mirrored), levels = c(-1, 0, 1), las = 2, main = "PIP.mirrored",
               xlab = expression("height h"),
               ylab = expression("height h'"))

par(pty="s")
TEP <- ifelse(PIP > 0 & PIP.mirrored > 0, 1, ifelse(PIP < 0 & PIP.mirrored < 0, -1, 0))

filled.contour(t(TEP), levels = c(-1.5, -0.5, 0.5, 1.5), col = c("red", "blue", "green"),
               plot.title={
                 title(main = "PIP and PIP.mirrored superimposed")
                 title(xlab=expression("height h"))
                 title(ylab=expression("height h'"))
                 abline(0, 1, lwd = 1)
               })






