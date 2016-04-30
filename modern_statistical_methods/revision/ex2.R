f <- function(u) { asin(u) }
g <- function(x) { 1 - sqrt(1 - x) }

sim.triangle <- function(n) {
  u <- 0
  i <- 0
  
  while(i < n) {
    x <- runif(1, 0, 1)
    y <- runif(1, 0, 2)
    
    if(y < 2 - 2*x) {
      u[i] = x
      i <- i + 1
    }
  }
  
  u
}

sim.binom <- function(N, n, p) {
  t <- numeric(n + 1)
  t[1] <- choose(n, 0) * (1 - p)^(n)
  for(i in 1:n) {
    t[i+1] <- t[i] + choose(n, i) * p^(i) * (1 - p)^(n - i)
  }

  findInterval(runif(N), t)
}

sim.binom.sum.bernoulli <- function(N, n, p) {
  u <- 0
  for (i in 1:N) {
    u[i] <- sum(runif(n) < p)
  }
  
  u
}

# doesn't work over full range for u ~ uniform(0, 1)?
inv.epanech <- function(u) {
  ((3 - 16*u)/16)^(1/3)
}

epanech.pdf <- function(x) {
  3/4 * (1 - x^2)
}

sim.epanech <- function(n) {
  u <- 0
  i <- 0
  
  while(i < n) {
    x <- runif(1, -1, 1)
    y <- runif(1, 0, 0.75)
    
    if(y < epanech.pdf(x)) {
      u[i] = x
      i <- i + 1
    }
  }
  
  u
}

sim.epanech.2 <- function(n) {
  x <- numeric(n)
  for(i in 1:n) {
    u <- runif(3, -1, 1)
    if(abs(u[3]) >= abs(u[2]) & abs(u[3]) >= abs(u[1])) {
      x[i] <- u[2]
    } else {
      x[i] <- u[3]
    }
  }
  
  x
}

sim.std.normal <- function(n) {
  x <- 0
  i <- 0
  
  while(i < n) {
    u <- runif(1, 0, 1)
    v <- runif(1, -1, 1)
    
    if(v^2 < -4 * u^2 * log(u)) {
      x[i] = v/u
      i <- i + 1
    }
  }
  
  x
}


