#### Newton-Raphson implementation for logistic regression
#### Medical Statistics Assignment, MSc Applied Statistics
#### James Aley

# The 'infert' dataset contains data about infertility after induced and
# spontaneous abortions. The data are given with a binary 0/1 response for
# cases vs controls.

# To start with, let's use R's implementation in the GLM function with this
# dataset, so that we have a golden answer to compare our algorithm to.

reference.model <- glm(case ~ factor(induced) + factor(spontaneous), 
                       data = infert,
                       family = "binomial")

reference.model$coefficients

### Our implementation of the N-R algorithm should produce coefficients
### quite similar to these, although we note that R is actually using
### a different algorithm (Fisher Scoring)

#  (Intercept)     factor(induced)1     factor(induced)2 factor(spontaneous)1 
# -1.7442332            0.4608354            0.8249893            1.2892855 
# factor(spontaneous)2 
# 2.3537835 


### To implement the N-R algorithm, we will need some extra functions:
### The sigmoid function for the logit link, plus the first and second
### derivatives of the binomial log-likelihood function.

#' Sigmoid function: e^x / 1+e^x
sigmoid <- function(x) {
  (exp(x) / (1 + exp(x)))
}

#' First derivative of the binomial log-likelihood function. 
#' Data should be a matrix where each row has the form:
#'  X, N, <explanatory variables>
#'  
#' @param beta the point (vector) to calculate the derivative at
#' @return a vector for the first derivative calculated at beta
binom.log.likelihood.prime <- function(data, beta) {
  p = length(beta)
  s = matrix(rep(0, p), ncol = 1, nrow = p)
  for(i in 1:nrow(data)) {
    r = data[i, ]
    a <- r[1]
    n <- r[2]
    x <- tail(r, n=-2)
    
    s = s + (x %*% (a - (n * sigmoid(t(x) %*% beta))))
  }
  
  s
}

#' Second derivative of the binomial log-likelihood function.
#' Data should be a matrix where each row has the form:
#'  X, N, <explanatory variables>
#'  
#' @param beta the point (vector) to calculate the second derivative at
#' @return a matrix containing the second derivative at point beta
binom.log.likelihood.prime2 <- function(data, beta) {
  p = length(beta)
  s = matrix(rep(0, p*p), ncol = p, nrow = p)
  for(i in 1:nrow(data)) {
    r = data[i, ]
    a <- r[1]
    n <- r[2]
    x <- tail(r, n=-2)
    
    s = s + (n * sigmoid(t(x) %*% beta)[,1] 
             * (1 - sigmoid(t(x) %*% beta))[,1] 
             * (x %*% t(x)))
  }
  
  -1 * s
}

#' Implementation of the Newton-Raphson algorithm. 
#'  
#' @param x0 A starting value for the algorithm
#' @param data A matrix where each row is: X, N, <explanatory variables>
#' @param ll.prime First derivative of log-likelihood function
#' @param ll.prim2 Second derivative of log-likelihood function
#' @param max.iter Maximum number of iterations to try before returning
#' @return The optimised vector of coefficients
newton.raphson <- function(x0, data, ll.prime, ll.prime2, max.iter=100) {
  b = x0
  iter = 0
  for(i in 1:max.iter) {
    first.deriv = ll.prime(data, b)
    vnorm = norm(first.deriv, "f")
    if(vnorm < 0.00001) {
      print(sprintf("Converged after %d iterations", i))
      return(b)
    } else {
      second.deriv = ll.prime2(data, b)
      b = b - solve(second.deriv) %*% first.deriv
    }
  }
  
  warning("Algorithm did not converge within max iterations.")
  return(b)
}

### Now let's test the algorithm:

# Set up some test data, we'll use the design matrix from
# reference.model, so that we know what coefficients the algorithm
# *should* produce if it's working properly:

# > d <- cbind(infert$case, rep(1, nrow(infert)), 
#              model.matrix(reference.model))

# > newton.raphson(c(1, 0, 0, 0, 0), d, 
#                  binom.log.likelihood.prime, 
#                  binom.log.likelihood.prime)
#
# [1] "Converged after 5 iterations"
#                            [,1]
# (Intercept)          -1.7442332
# factor(induced)1      0.4608354
# factor(induced)2      0.8249893
# factor(spontaneous)1  1.2892855
# factor(spontaneous)2  2.3537835

### These are very similar to the values given in the model we fitted 
### with the GLM function above:

# > newton.raphson(c(1, 0, 0, 0, 0), d, 
#                  binom.log.likelihood.prime, 
#                  binom.log.likelihood.prime)
#   - reference.model$coefficients
#
# [1] "Converged after 5 iterations"
#                               [,1]
# (Intercept)           2.141562e-08
# factor(induced)1     -4.881931e-09
# factor(induced)2     -1.382456e-08
# factor(spontaneous)1 -1.876198e-08
# factor(spontaneous)2 -2.065155e-08
