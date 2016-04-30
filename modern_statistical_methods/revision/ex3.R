# q1a - simulate std norm using CLT

sim.norm.clt <- function(n) {
  x <- numeric(n)
  for(i in 1:n) {
    u <- runif(12, 0, 1)
    x[i] <- sum(u)
  }
  
  (x - 6)
}

# q1b - simulate std norm using box muller method

sim.norm.box.muller <- function(n) {
  u1 <- runif(n/2)
  u2 <- runif(n/2)
  r <- sqrt(-2 * log(u1))
  theta <- 2 * pi * u2
  x = r * cos(theta)
  y = r * sin(theta)
  
  c(x, y)
}

# q1c - polar marsaglia method

sim.norm.polar.marsaglia <- function(n) {
  v1 <- runif(n/2, -1, 1)
  v2 <- runif(n/2, -1, 1)
  
  s <- v1^2 + v2^2
  k <- sqrt(-2 * log(s) / s)
  c(v1 * k, v2 * k)
}

sim.norm.box.muller.nicolas <- function(n) {
  t=0
  X=0
  Y=0
  while(t<n/2)
  {U1=runif(1,0,1)
   U2=runif(1,0,1)
   S=-2*log(U1)
   Theta=2*pi*U2
   X[t]=sqrt(S)*cos(Theta)
   Y[t]=sqrt(S)*sin(Theta)
   t=t+1}
  Z=c(X,Y)
}

## helper for timing
time.fun <- function(f, n) {
  start <- proc.time()
  f(n)
  proc.time() - start
}

# q2 - beta distribution

beta.pdf <- function(a, b, x) {
  k <- gamma(a + b) / (gamma(a) * gamma(b))
  k * x^(a-1) * (1 - x)^(b-1)
}

# test plot
# plot(function(x) {beta.pdf(8, 6, x)}, 0, 1)

sim.beta.rejection <- function(n, a, b) {
  x <- numeric(n)
  while(i < n) {
    u1 <- runif(1)
    u2 <- runif(1, 0, 3)
    
    if(u2 < beta.pdf(a, b, u1)) {
      x[i] <- u1
      i <- i + 1
    }
  }
  
  x
}

sim.beta.gamma <- function(n, a, b) {
  theta = 1
  x <- rgamma(n, a, theta)
  y <- rgamma(n, b, theta)
  x / (x + y)
}

# q3
# (a)
x1 <- rpois(10000, 2)
x2 <- rpois(10000, 3)
x3 <- rpois(10000, 4)
x4 <- rpois(10000, 5)

hist(x1 + x2 + x3 + x4)
mean(x1 + x2 + x3 + x4)

# (b)
x1 <- rcauchy(10000)
x2 <- rcauchy(10000)
x3 <- rcauchy(10000)
x4 <- rcauchy(10000)

hist(1/4 * ((x1 + x2 + x3 + x4)))
mean(x1 + x2 + x3 + x4)
var(x1 + x2 + x3 + x4)

# (c)
x1 <- rnorm(10000)
x2 <- rnorm(10000)
x3 <- rnorm(10000)
x4 <- rnorm(10000)

hist(abs(x1 * x2 + x3 * x4))
mean(abs(x1 * x2 + x3 * x4))
var(abs(x1 * x2 + x3 * x4))

# q4

# crude/raw
q4.crude <- function(n) {
  y = runif(n)
  mean((2*y)^(2*y)) * 2  
}

# antithetic variates
q4.antithetic <- function(n) {
  y <- runif(n)
  f <- function(y) {
    (2*y)^(2*y)
  }
  mean(0.5 * (f(y) + f(1-y))) * 2
}

# hit/miss
q4.hitmiss <- function(n) {
  x <- runif(n, 0, 2)
  y <- runif(n, 0, 4) # max for x^x in (0, 2)
  (sum(y < x^x) / n) * 8 # rectangle is 2x4
}

# q5

check.clt <- function(lambda) { 
  f <- function(n, lambda) {
    replicate(10000, sum(rpois(n, lambda)) / n)
  }
  
  par(mfrow=c(2,3))
  hist(f(5, lambda), main="n=5")
  hist(f(10, lambda), main="n=10")
  hist(f(20, lambda), main="n=20")
  hist(f(30, lambda), main="n=30")
  hist(f(50, lambda), main="n=50")
  hist(f(100, lambda), main="n=100")
}

check.clt(0.2)  # somewhat normal at n=100
check.clt(1)    # quite good at n=10, 20
check.clt(5)    # good at n=10
check.clt(25)   # ok at n=5

# q6

A <- c(0, 8, 11, 15)
B <- c(0, 1, 2, 4)

t_obs <- mean(A) - mean(B)
total <- sum(c(A, B))
rotations <- combn(c(A, B), 4)
diffs <- numeric(ncol(rotations))

for(i in 1:ncol(rotations)) {
  diffs[i] <- mean(rotations[, i]) - ((total - sum(rotations[, i])) / 4)
}

p.value <- sum(diffs >= t_obs) / length(diffs)
