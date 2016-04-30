# MSM Ex 4


# Q1
# (a)

q1.f <- function(x) {
   exp(x^2)
}

# crude estimate for integral f(x) over [0, 1]
q1.crude <- function(n) {
  sum(exp(runif(n)^2)) / n
}

