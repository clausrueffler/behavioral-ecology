w.H <- function(p){
  p * 1/2 * (V - C) + (1 - p) * V
}

w.D <- function(p){
  (1-p) * 1/2 * V
}

w <- function(p.m, p.r) {
  p.m * w.H(p.r) + (1-p.m) * w.D(p.r)
}

V <- 2
C <- 3

w(0.35, 0.35) # payoff of a resident against itself
1/2 * (V - C * 0.35^2) # explicit formula for payoff of residenet against itself

w(0.5, 0.35) # payoff of a mutant against resident
1/2 * V * (0.5 - 0.35) - 1/2 * C * 0.5 * 0.35 + 1/2 * V # expicit formula for payoff of mutant against resident

s <- function(p.m, p.r) {
  w(p.m, p.r) - w(p.r, p.r)
}

s(0.25, 0.75)
s(0.75, 0.25)

s(0.66, 0.33)
s(0.33, 0.66)

s(0.9, 0.99)
s(0.99, 0.9)

s(0.67, 0.01)
s(0.01, 0.67)

s(0.01, 0.02)
s(0.02, 0.01)

s(0.01, 0.99)
s(0.99, 0.01)

s(0.65, 0.67)
s(0.67, 0.65)
