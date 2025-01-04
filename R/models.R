#' Choice Models for Effort-Based Decision Making
#'
#' @description
#' This script implements computational models for effort-based choice behavior.
#' Models include:
#' - Baseline model (mBASE): Simple effort sensitivity
#' - Heuristic model (mHRST): Rule-based choices
#' - Convex models (mHYPER, mSENS): Hyperbolic and exponential effort discounting
#' - Concave models (mPOWER, mSIGM): Power and sigmoidal effort functions
#' - Dual-system models (mDEXPO, mDPOWER): Combined positive/negative effort components
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-05-12
#' @copyright (c) 2023 Przemyslaw Marcowski

#' Baseline Choice Model
#'
#' Simple model with single parameter for effort sensitivity bias.
#'
#' @param par Vector of length 1 containing bias parameter
#' @param d Data frame with EffortfulOptionChosen column
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and differences or negative log-likelihood
mBASE_lim <- matrix(c(-1e+2, 1e+2), nrow = 2)
mBASE <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

#' Heuristic Choice Model
#'
#' Choice model based on simple decision rules using reward and effort differences.
#'
#' @param par Vector of length 5: gain, loss, effort diff, effort ratio, and temperature
#' @param d Data frame with G, R, D, E columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and differences or negative log-likelihood
mHRST_lim <- matrix(c(-Inf, Inf, -Inf, Inf, -Inf, Inf, -Inf, Inf, 1e-7, 1e+3), nrow = 2)
mHRST <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

# Convex Models -----------------------------------------------------------

#' Generalized Hyperbolic Model
#'
#' Model with hyperbolic discounting of effort.
#'
#' @param par Vector of length 3: discount rate, sensitivity, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mHYPER_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mHYPER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

#' Constant-Sensitivity Model
#'
#' Model with exponential discounting of effort.
#'
#' @param par Vector of length 3: discount rate, sensitivity, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mSENS_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mSENS <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

# Concave Models ----------------------------------------------------------

#' Flexible Power Model
#'
#' Model with power discounting of effort.
#'
#' @param par Vector of length 3: discount rate, sensitivity, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mPOWER_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mPOWER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

#' Sigmoidal Model
#'
#' Model with sigmoidal discounting of effort.
#'
#' @param par Vector of length 3: discount rate, sensitivity, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mSIGM_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+3), nrow = 2)
mSIGM <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

# Dual-System Models ------------------------------------------------------

#' Double-Exponential Model
#'
#' Model with double-exponential discounting of effort.
#'
#' @param par Vector of length 4: discount rates, mixing parameter, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mDEXPO_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1 - 1e-7, 1e-7, 1e+3), nrow = 2)
mDEXPO <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
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

#' Dual-Power Model
#'
#' Model with dual power discounting of effort.
#'
#' @param par Vector of length 6: discount rates, sensitivities, mixing parameter, and temperature
#' @param d Data frame with X1, X2, E1, E2 columns
#' @param choice Return probabilities if TRUE, neg log-likelihood if FALSE
#' @param epsilon Smoothing parameter (0,1)
#' @param lambda Regularization parameter
#' @return Matrix of probabilities and values or negative log-likelihood
mDPOWER_lim <- matrix(c(1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1e+2, 1e-7, 1 - 1e-7, 1e-7, 1e+3), nrow = 2)
mDPOWER <- function(par, d, choice = FALSE, epsilon = 0.001, lambda = 0.001) {
  # Compute option values
  sv2 <- d$X2 * (1 + (par[5] * (par[1] * d$E2^par[3]) - (1 - par[5]) * (par[2] * d$E2^par[4])))
  sv1 <- d$X1 * (1 + (par[5] * (par[1] * d$E1^par[3]) - (1 - par[5]) * (par[2] * d$E1^par[4])))

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
