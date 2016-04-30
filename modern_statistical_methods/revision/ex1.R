## (1)
target = pi^2 / 6

# Function to calculate approximation
f <- function(N) {
  sum(1 / (1:N)^2)
}

# Calculate % error after n iterations
sapply(c(5, 50, 500, 5000), function(n) {(target - f(n)) / target})

## (2)

v <- runif(1000)
q <- list(c(0, 0.25), c(0.25, 0.5), c(0.5, 0.75), c(0.75, 1))

sapply(q, function(limits) {
  sum(v >= limits[1] & v < limits[2])
})

## (3)

total <- 0
count <- 0

for(i in runif(1000)) {
  if(total > 100) {
    break;
  } else {
    total <- total + i
    count <- count + 1
  }
}

# answer: 201

## (4)

f <- function(x) {
  x^4 + 3*x^3 - 2*x^2 - 1
}

fprime <- function(x) {
  4*x^3 + 9*x^2 - 4*x
}

x0 <- 1
xn <- x0
while(f(xn) > 0.000005) {
  xn <- xn - (f(xn) / fprime(xn))
}

# answer: 086414

## (5):

roll <- function(n) {
  as.integer(runif(n) * 6) + 1
}

