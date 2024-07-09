# Title: Effort-based choice model functions
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-05-12
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Choice models -----------------------------------------------------------

mBASE_lim <- matrix(c(-1e+2, 1e+2), nrow = 2)
mBASE <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Baseline model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute the difference in option values
  diff <- par[1]
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-diff))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return the probability and the difference
  if (choice == TRUE) {
    return(cbind(P2, diff))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

## Heuristic ----

mHRST_lim <- matrix(c(-Inf, Inf, -Inf, Inf, -Inf, Inf, -Inf, Inf, 1e-7, 1e+3), nrow = 2)
mHRST <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Choice heuristic model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute the difference in option values
  diff <- (par[1] * d$G + par[2] * d$R + par[3] * d$D + par[4] * d$E)
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[5] * diff))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return the probability and the difference
  if (choice == TRUE) {
    return(cbind(P2, diff))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

## Convex models ----

mHYPER_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mHYPER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Generalized Hyperbolic model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * (1 / (1 + par[1] * d$E2)^par[2])
  sv1 <- d$X1 * (1 / (1 + par[1] * d$E1)^par[2])
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[3] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

mSENS_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mSENS <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Constant-Sensitivity model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * (exp(-(par[1] * d$E2)^par[2]))
  sv1 <- d$X1 * (exp(-(par[1] * d$E1)^par[2]))
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[3] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

## Concave models ----

mPOWER_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mPOWER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Flexible Power model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * (1 - par[1] * d$E2^par[2])
  sv1 <- d$X1 * (1 - par[1] * d$E1^par[2])
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[3] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

mSIGM_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mSIGM <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Sigmoidal model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * (1 - ((1 / (1 + exp(-par[1] * (d$E2 - par[2])))) - (1 / (1 + exp(par[1] * par[2])))) * (1 + (1 / (exp(par[1] * par[2])))))
  sv1 <- d$X1 * (1 - ((1 / (1 + exp(-par[1] * (d$E1 - par[2])))) - (1 / (1 + exp(par[1] * par[2])))) * (1 + (1 / (exp(par[1] * par[2])))))
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[3] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

## Dual-System models ----

mDEXPO_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1 - 1e-7, 1e-7, 1e+3), nrow = 2)
mDEXPO <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Double-Exponential model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * ((par[3] * exp(-par[1] * d$E2)) + ((1 - par[3]) * exp(-par[2] * d$E2)))
  sv1 <- d$X1 * ((par[3] * exp(-par[1] * d$E1)) + ((1 - par[3]) * exp(-par[2] * d$E1)))
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[4] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}

mDPOWER_lim <- matrix(c(-1e+2, -1e-7, 1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1 - 1e-7, 1e-7, 1e+3), nrow = 2)
mDPOWER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Dual-Power model function
  #
  # Args:
  #   par: Numeric vector of parameters. For this function, `par` should be of length 1.
  #   d: Data frame with a column EffortfulOptionChosen.
  #   choice: Boolean indicating whether to return the probability and value.
  #   epsilon: Numeric smoothing parameter between 0 and 1.
  #   lambda: Numeric regularization parameter.
  #
  # Returns:
  #   If choice=TRUE, a matrix with columns P2 and diff.
  #   If choice=FALSE, a numeric representing the negative log-likelihood.
  
  # Compute option values
  sv2 <- d$X2 * (1 - (par[5] * (par[1] * d$E2^par[3]) + (1 - par[5]) * (par[2] * d$E2^par[4])))
  sv1 <- d$X1 * (1 - (par[5] * (par[1] * d$E1^par[3]) + (1 - par[5]) * (par[2] * d$E1^par[4])))
  
  # Compute choice probability
  P2 <- 1 / (1 + exp(-par[6] * (sv2 - sv1)))
  
  # Apply smoothing to avoid absolute certainty
  P2 <- epsilon * 0.5 + (1 - epsilon) * P2
  
  # If choice=TRUE, return probabilities and values
  if (choice == TRUE) {
    return(cbind(P2, sv1, sv2))
  } else {
    # Otherwise, compute and return estimator
    P <- ifelse(as.logical(d$EffortfulOptionChosen), P2, 1 - P2)
    nll <- -sum(log(P)) # negative log likelihood
    # nlp <- -sum(dnorm(par, mean = 0, sd = 1, log = TRUE)) # negative log prior
    # reg <- lambda * sum(par^2) # regularization term
    # nlp <- nll + nlp + reg # negative log posterior with regularization
    return(nll)
  }
}
