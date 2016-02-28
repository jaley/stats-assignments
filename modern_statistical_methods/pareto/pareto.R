## MSM Assigment code.

#' Probability density function for the Paretro distribution, with
#' parameters alpha and beta
pareto.pdf <- function(alpha, beta, x) {
  (alpha * beta*alpha) / (x + beta)^(alpha + 1)
}

#' Cumulative distribution funtion for Paretro distribution with paremeters
#' alpha abd beta.
pareto.cdf <- function(alpha, beta, u) {
  1 - (beta / (u + beta))^alpha
}

#' Inverse cumulative distribution function for the Pareto
#' distribution. Alpha and Beta are distribution parameters,
#' q should be a value between 0 and 1.
pareto.inv.cdf <- function(alpha, beta, q) { 
  beta * ((1 - q)^(-1/alpha) - 1) 
}

## Compare simulated values to the density function

