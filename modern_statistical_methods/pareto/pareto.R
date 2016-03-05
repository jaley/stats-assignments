## MSM Assigment code.

#' Probability density function for the Paretro distribution, with
#' parameters alpha and beta
pareto.pdf <- function(alpha, beta, x) {
  (alpha * beta^alpha) / (x + beta)^(alpha + 1)
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
require(ggplot2)

pareto <- data.frame(simulated = pareto.inv.cdf(3, 100000, runif(10000)))

ggplot(pareto, aes(simulated)) + 
  geom_density(colour="red", fill="red", alpha=0.1) + 
  stat_function(fun = function(x) {pareto.pdf(3, 100000, x)},
                colour="blue", geom="area", alpha=0.1, fill="blue") +
  xlab("X") +
  ylab("Density") +
  xlim(0, 100000)

## Question 2

#' Returns a vector of length n_runs containing simulated values
#' for the year end assets.
simulate.year.end.assets <- function(n_runs = 10000,
                                     start_assets = 250000,
                                     n_customers = 1000,
                                     premium = 6000,
                                     claim_prob = 0.1) {
  # Accumulate year-end assets in a vector
  outcomes <- numeric(length = n_runs)
  for(i in 1:n_runs) {
    # Number of claims this year, in this simulation
    n_claims <- rbinom(1, n_customers, claim_prob)
    
    # Calculate year-end profit/loss for this number of claims, by
    # drawing from Pareto distribution
    outcomes[i] <- start_assets + 
      (n_customers * premium) - 
      sum(pareto.inv.cdf(3, 100000, runif(n_claims)))  
  }
  return(outcomes)
}

# Reformat simulation data into a data frame for plotting
# a histogram of the simulation outcomes
bankruptcy <- data.frame(assets=simulate.year.end.assets())
ggplot(bankruptcy, aes(assets)) + 
  geom_density(colour="blue", fill="blue", alpha=0.1) + 
  geom_vline(xintercept = 0, linetype="longdash") +
  xlab("Assets at Year End") +
  ylab("Probability Density")

# Calculate the probability of bankruptcy, i.e. the proportion
# of outcomes where year end profits were less than zero.
sum(bankruptcy['assets'] < 0) / n_runs

## Question 3

#' Esimate the probability of bankruptcy for a premium of given price
bankruptcy.prob <- function(premium) {
  N <- 10000
  assets <- simulate.year.end.assets(premium=premium, n_runs=N)
  sum(assets < 0) / N
}

premiums <- seq(from=5500, to=8000, by=250)
premium.data <- data.frame(premium.price = premiums,
                           bankruptcy.prob = sapply(premiums, bankruptcy.prob))

ggplot(premium.data, aes(x=premium.price, y=bankruptcy.prob)) +
  geom_line(colour="blue") +
  stat_hline(yintercept = 0.02, linetype="longdash") +
  xlab("Premium Price (GBP)") +
  ylab("Probability of Bankruptcy")

## (b)

bankruptcy.prob.claim <- function(claim.prob) {
  N <- 10000
  assets <- simulate.year.end.assets(claim_prob = claim.prob, n_runs=N)
  sum(assets < 0) / N  
}

claims <- seq(from=0.05, to=0.15, by=0.005)
claim.data <- data.frame(claim.prob = claims,
                         bankruptcy.prob = 
                           sapply(claims, bankruptcy.prob.claim))

ggplot(claim.data, aes(x=claim.prob, y=bankruptcy.prob)) +
  geom_line(colour="blue") +
  stat_hline(yintercept = 0.02, linetype="longdash") +
  xlab("Claim Probability") +
  ylab("Probability of Bankruptcy")

