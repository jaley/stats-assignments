### MSM Ex 4

## Q1

# (a)

q1.f <- function(x) {
   exp(x^2)
}

# crude estimate for integral f(x) over [0, 1]
q1.crude <- function(n) {
  sum(q1.f(runif(n))) / n
}

# (b) using importance function f(x) = 3x^2

q1.importance <- function(n) {
  u <- runif(n)
  x <- u^(1/3)
  1 + (sum((q1.f(x) - 1) / (3 * x^2)) / n)
}

# Justification: 3x^2 is a PDF we can easily invert to simulate. 
# exp(x^2) - 1 is roughly proportional to 3x^2, so makes it a good importance function

# (c) using control variate y = 2x

# first need to estimate covariance and calculate minimizing constant:
u <- runif(100)
x <- exp(u^2)
y <- 2 * u
c <- - cov(x, y) / var(y)

# Now use this in simulation:

q1.control <- function(n) {
  u <- runif(n)
  x <- exp(u^2)
  y <- 2 * u
  sum(x + c * (y - 1)) / n
}

# (d) compare functions above

for(f in c(q1.crude,
           q1.importance,
           q1.control)) {
  samples <- replicate(100, f(100))
  print(c(mean(samples), var(samples)))
}

# Importance sampling produces lowest variance estimator by a long way

## Q2

# using 100k samples for all simulations in Q2
q2.n <- 100000

q2.sim.morgan <- function(t) {
  c <- pi/sqrt(3)
  u <- runif(q2.n)
  r <- (1 + exp(-t * c)) / u
  x <- -log(r-1)/c
  phi <- exp(-x*x/2)/sqrt(2*pi)
  q <- phi * (1 + exp(-x * c))^2 * exp(x * c)
  sum(q)/(q2.n * (c * (1 + exp(-t * c))))
}

q2.sim.exp <- function(t) {
  u <- runif(q2.n)
  v <- t - log(u) 
  phi <- (1/sqrt(2*pi)) * exp(0.5 * v * v)
  f <- exp(t - v)
  sum(phi/f) / q2.n
}

# Q4

# (a)

vaccine.n <- 8197
placebo.n <- 8198
vaccine.aids <- 51
placebo.aids <- 74

theta <- (vaccine.aids/vaccine.n) / (placebo.aids/placebo.n)

# H0: theta >= 1
# H1: theta < 1

q3.boostrap <- function(n) {
  vaccine <- c(rep(1, vaccine.aids), rep(0, vaccine.n - vaccine.aids))
  placebo <- c(rep(1, placebo.aids), rep(0, placebo.n - placebo.aids))
  
  replicate(n, mean(sample(vaccine, replace = T)) / 
              mean(sample(placebo, replace = T)))
}

# calculate a p-value at 95% level

q3.p.value <- mean(q3.boostrap(10000) >= 1)

# p-value ~= 0.02, so we reject H0 in favour of H1, that alpha is lower than beta

# traditional t-test results:
t.test(c(rep(1, vaccine.aids), rep(0, vaccine.n - vaccine.aids)),
       c(rep(1, placebo.aids), rep(0, placebo.n - placebo.aids)),
       alternative = "less")

# p-value ~= 0.02, so still reject at 95% level

## Q4

# (a)

# load the data first
d <- read.csv("hormones.csv", header=T)

q4.bootstrap <- function(n) {
  theta <- numeric(n)
  for(i in 1:n) {
    resample <- d[sample(nrow(d), replace=T), ]
    theta[i] <- (mean(resample$Newpatch) - mean(resample$Oldpatch)) /
                (mean(resample$Oldpatch) - mean(resample$Placebo))
  }
  
  theta
}

b <- q4.bootstrap(10000)

theta.est <- mean(b) # -0.06461039
theta.se <- sd(b)    # 0.103125
theta.ci <- quantile(b, probs = c(0.025, 0.975))
theta.pop <- (mean(d$Newpatch) - mean(d$Oldpatch)) /
             (mean(d$Oldpatch) - mean(d$Placebo))
theta.bias <- mean(b) - theta.pop

# No reason to reject H0 (bioequivalence of patches)

