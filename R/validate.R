# Title: Toolbox for model evaluation
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-05-12
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Data sampling ----------------------------------------------------------------

split_data_mccv <- function(d, p) {
  # Subsamples data into training and test set according to proportion p.
  # Performs Monte Carlo Cross-Validation if p < 1.
  #
  # Args:
  #   d: Data to split
  #   p: Proportion for splitting data d into training and test set
  #
  # Returns:
  #   Training and test data set. Test set is equal to training if p = 1.
  if (p < 1) {
    nchos <- nrow(d)
    nfset <- floor(nchos * p)
    fset <- sample(1:nchos, nfset)
    return(list(d[fset, ], d[(1:nchos)[!(1:nchos) %in% fset], ]))
  } else {
    return(list(d, d))
  }
}

split_data_kfold <- function(d, k, r, shuffle = TRUE) {
  # Splits data into k folds. 
  # Model is trained on k - 1 of the folds and validated using remaining data.
  # Performs LOOCV if k equals the number of data points.
  #
  # Args:
  #   d: Data to split
  #   k: Number of folds to split data into
  #   r: Current validation repetition
  #   shuffle: Logical. If TRUE, reshuffles data on loop start.
  #            Default is TRUE.
  #
  # Returns:
  #   k - 1 folds as training data set and remaining data as test set
  if (k == 1) return(list(d, d))
  else if (shuffle == TRUE) if (r %% k == 1) d <- d[sample(nrow(d)), ]
  nchos <- nrow(d)
  nfset <- rep(1:k, diff(floor(nchos * seq(0, 1, length.out = k + 1))))
  fset <- split(d, nfset)
  return(list(do.call(rbind, fset[-(r %% k + 1)]), fset[[(r %% k + 1)]]))
}

split_data_nested_kfold <- function(d, k_outer, k_inner, r_outer, r_inner, shuffle = TRUE) {
  # Splits data into k_outer folds for main validation and k_inner folds for hyperparameter tuning.
  #
  # Args:
  #   d: Data to split
  #   k_outer: Number of outer folds to split data into for main validation
  #   k_inner: Number of inner folds to split training data into for hyperparameter tuning
  #   r_outer: Current outer validation repetition
  #   r_inner: Current inner validation repetition
  #   shuffle: Logical. If TRUE, reshuffles data on loop start.
  #            Default is TRUE.
  #
  # Returns:
  #   k_outer - 1 folds as training data set and remaining data as test set, with training data further split into k_inner folds
  
  if (k_outer == 1 && k_inner == 1) return(list(d, d))
  
  else if (shuffle == TRUE) {
    if (r_outer %% k_outer == 1) d <- d[sample(nrow(d)), ]
  }
  
  nchos_outer <- nrow(d)
  nfset_outer <- rep(1:k_outer, diff(floor(nchos_outer * seq(0, 1, length.out = k_outer + 1))))
  fset_outer <- split(d, nfset_outer)
  
  train_data <- fset_outer[-(r_outer %% k_outer + 1)]
  test_data <- fset_outer[[(r_outer %% k_outer + 1)]]
  
  if (shuffle == TRUE) {
    if (r_inner %% k_inner == 1) train_data <- train_data[sample(nrow(train_data)), ]
  }
  
  nchos_inner <- nrow(train_data)
  nfset_inner <- rep(1:k_inner, diff(floor(nchos_inner * seq(0, 1, length.out = k_inner + 1))))
  fset_inner <- split(train_data, nfset_inner)
  
  train_data_inner <- fset_inner[-(r_inner %% k_inner + 1)]
  valid_data_inner <- fset_inner[[(r_inner %% k_inner + 1)]]
  
  return(list(train_data_inner, valid_data_inner, test_data))
}

split_data_boot <- function(d, v = TRUE) {
  # Resamples dataset with replacement.
  #
  # Args:
  #   d: Data to resample
  #   v: If TRUE, model is validated using OOB data.
  #      If FALSE, original data is used.
  #      Default is TRUE.
  # 
  # Returns:
  #   Training and test data sets
  nchos <- nrow(d)
  fset <- sample(1:nchos, replace = TRUE)
  if (v == TRUE) {
    return(list(d[fset, ], d[(1:nchos)[!(1:nchos) %in% fset], ]))
  } else {
    return(list(d[fset, ], d))
  }
}

split_data <- function(d, method, ...) {
  if (method == "MCCV") {
    dout <- split_data_MCCV(d, ...)
  } else if (method == "kfold") {
    dout <- split_data_kfold(d, ...)
  } else if (method == "boot") {
    dout <- split_data_boot(d, ...)
  } else {
    stop("'method' can only be 'mccv', 'kfold', or 'boot'")
  }
  return(dout)
}

# Starting values ---------------------------------------------------------

draw_starts <- function(x) {
  # Draws start values for each model parameter from existing set
  #
  # Args:
  #   x: Model string for which parameters are drawn
  #
  # Returns:
  #   Starting value for each model parameter drawn from set
  start <- c()
  for (i in 1:ncol(x)) start[i] <- sample(x[, i], 1)
  return(start)
}

rand_starts <- function(m) {
  # Randomizes start values for each model parameter within pre-defined limits
  #
  # Args:
  #   x: Model string for which parameters are randomized
  #
  # Returns:
  #   Randomized starting value for each model parameter
  start <- c()
  lim <- lims[[m]]
  for (i in 1:ncol(lim)) {
    start[i] <- runif(
      1,
      ifelse(lim[1, i] < -1e+10, -1e+10, lim[1, i]),
      ifelse(lim[2, i] > 1e+10, 1e+10, lim[2, i])
    )
  }
  return(start)
}

get_starts <- function(m, np, dx, ...) {
  # Fits a model to data once and saves parameter estimates to drive. 
  #
  # Args:
  #   m: Model string for which parameters are to be estimated
  #   np: Number of model parameters
  #
  # Returns:
  #   Parameter values. Parameter estimates are also saved to drive.
  par <- lapply(dx, function(x) {
    run <- 0
    fit <- FALSE
    starts <- rep(1, np)
    while (fit == FALSE) {
      run <- run + 1
      res_tmp <- try(if (run == 1) {
        optim(
          starts, get(m),
          d = x, ...,
          method = ifelse(np > 1, "L-BFGS-B", "Brent"),
          lower = lims[[m]][1, ], upper = lims[[m]][2, ]
        )
      } else {
        optim(
          rand_starts(m), get(m),
          d = x, ...,
          method = ifelse(np > 1, "L-BFGS-B", "Brent"),
          lower = lims[[m]][1, ], upper = lims[[m]][2, ]
        )
      })
      if (!class(res_tmp) == "try-error") fit <- TRUE
    }
    return(res_tmp$par)
    }
  )
  pars <- do.call(rbind, par)
  return(pars)
}

# Model fitting -----------------------------------------------------------

get_fit_optim <- function(m, np, dfit, nfit, ...) {
  # Fits model using L-BFGS-B (or Brent for one-dimensional models).
  # Fitting is performed by estimator MINIMIZATION.
  # Fitting is repeated until nfit successful fits are obtained.
  # Uses regularization if epsilon is supplied.
  #
  # Args:
  #   m: Model string for model to be fitted.
  #   np: number of model parameters.
  #   dfit: Data for fitting.
  #   nfit: Number of fits performed to get best fit.
  #   ...: Arguments passed to inner functions.
  #
  # Returns:
  #   Estimator value resulting from best fit.
  #   Parameter values resulting from best fit.
  fit_v <- Inf
  fit_c <- 0
  while (fit_c < nfit) {
    res_tmp <- try(
      optim(
        draw_starts(starts[[m]]), get(m),
        d = dfit, ...,
        method = ifelse(np > 1, "L-BFGS-B", "Brent"),
        lower = lims[[m]][1, ], upper = lims[[m]][2, ]
      ), silent = FALSE)
    if (class(res_tmp) != "try-error") {
      fit_c <- fit_c + 1
      if (res_tmp$value < fit_v) {
        fit_v <- res_tmp$value
        fit_p <- res_tmp$par
      }
    } else {
      print("Error!")
    }
  }
  if (fit_v != -Inf) {
    return(c(fit_v, fit_p))
  } else {
    return(NULL)
  }
}

get_fit_de <- function(dfit, m, nfit, ...) {
  # Fits model using differential evolution algorithm.
  # Fitting is performed by estimator MINIMIZATION.
  # Fitting is repeated until nfit successful fits are obtained.
  # Uses regularization if epsilon is defined.
  # Pre-defined number of algorithm iterations niters is required.
  # Dependencies: DEoptim
  #
  # Args:
  #   dfit: Data for fitting.
  #   m: Model string for model to be fitted.
  #   nfit: Number of fits performed to get best fit.
  #   ...: Arguments passed to inner functions.
  #
  # Returns:
  #   Estimator value resulting from best fit.
  #   Parameter values resulting from best fit.
  require(DEoptim)
  fit_v <- Inf
  fit_c <- 0
  while (fit_c < nfit) {
    res_tmp <- try(DEoptim(
      get(m), d = dfit, ...,
      lower = lims[[m]][1, ], upper = lims[[m]][2, ],
      control = DEoptim.control(itermax = niters, trace = TRUE)
    ),
    silent = FALSE
    )
    if (class(res_tmp) != "try-error") {
      fit_c <- fit_c + 1
      if (res_tmp$optim$bestval < fit_v) {
        fit_v <- res_tmp$optim$bestval
        fit_p <- res_tmp$optim$bestmem
      }
    } else {
      print("Error!")
    }
  }
  if (fit_v != -Inf) {
    return(c(fit_v, fit_p))
  } else {
    return(NULL)
  }
}

get_fit_ga <- function(dfit, m, nfit, ...) {
  # Fits model using a genetic algorithm.
  # Fitting is performed by estimator MAXIMIZATION.
  # Fitting is repeated until nfit successful fits are obtained.
  # Uses regularization if epsilon is defined.
  # Pre-defined number of algorithm populations npops is required.
  # Pre-defined number of algorithm runs nruns is required.
  # Pre-defined number of algorithm iterations niters is required.
  # Dependencies: GA
  #
  # Args:
  #   dfit: Data for fitting.
  #   m: Model string for model to be fitted
  #   nfit: Number of fits performed to get best fit
  #   ...: Arguments passed to inner functions
  #
  # Returns:
  #   Estimator value resulting from best fit.
  #   Parameter values resulting from best fit.
  require(GA)
  fit_v <- -Inf
  fit_c <- 0
  while (fit_c < nfit) {
    res_tmp <- try(ga(
      type = "real-valued", fitness = get(m), d = dfit, ...,
      lower = lims[[m]][1, ], upper = lims[[m]][2, ],
      suggestions = draw_starts(starts[[m]]), get(m),
      popSize = npops, run = nruns, maxiter = niters
    ),
    silent = FALSE
    )
    if (class(res_tmp) != "try-error") {
      fit_c <- fit_c + 1
      if (res_tmp@fitnessValue > fit_v) {
        fit_v <- res_tmp@fitnessValue
        fit_p <- res_tmp@solution[1, ]
      }
    } else {
      print("Error!")
    }
  }
  if (fit_v != Inf) {
    return(c(fit_v, fit_p))
  } else {
    return(NULL)
  }
}

get_fit <- function(d, method, m, ...) {
  if (method == "optim") {
    fout <- get_fit_optim(d, m, nfit, ...)
  } else if (method == "de") {
    fout <- get_fit_de(d, m, nfit, ...)
  } else if (method == "ga") {
    fout <- get_fit_ga(d, m, ...)
  } else {
    stop("'method' can only be 'optim', 'de', or 'ga'")
  }
  return(fout)
}

# Model metrics -----------------------------------------------------------

get_results_discrete <- function(dpre, m, fit, ...) {
  # Extracts model training and test results in classification problems
  #
  # Args:
  #   dpre: Test data set
  #   m: Model string for model to be evaluated
  #   fit: Model fit
  #
  # Returns:
  #   Training model log-likelihood.
  #   Multiple loss functions values obtained based on test data set.
  #   Extremity abs(pred - .5). Number of NAs obtained during modeling.
  estim <- fit[1]
  pred <- eval(call(m, quote(fit[-1]), quote(dpre), choice = TRUE, quote(...)))
  logloss <- (1 / nrow(dpre)) * -sum(log(ifelse(dpre$EffortfulOptionChosen == 1, pred[, 1], 1 - pred[, 1])))
  z_o <- mean(abs(pred[, 1] - dpre$EffortfulOptionChosen), na.rm = TRUE)
  extr <- mean(abs(pred[, 1] - .5), na.rm = TRUE)
  nna <- sum(is.na(pred[, 1]))
  res <- cbind(estim, logloss, z_o, extr, nna)
  return(res)
}

get_results_regression <- function(dpre, m, fit, ...) {
  # Extracts model training and test results in regression problems
  #
  # Args:
  #   dpre: Test data set
  #   m: Model string for model to be evaluated
  #   fit: Model fit
  #
  # Returns:
  #   Training model log-likelihood.
  #   Multiple loss functions values obtained based on test data set.
  #   Extremity abs(pred - .5). Number of NAs obtained during modeling.
  estim <- fit[1]
  pred <- eval(call(m, quote(fit[-1]), quote(dpre), pred = TRUE, quote(...)))
  mse <- mean((dpre$y - pred)^2, na.rm = TRUE)
  mae <- mean(abs(dpre$y - pred), na.rm = TRUE)
  mape <- mean(abs((dpre$y - pred) / dpre$y)) * 100
  smape <- mean(2 * abs(dpre$y - pred) / (abs(dpre$y) + abs(pred)), na.rm = TRUE) * 100
  nna <- sum(is.na(pred))
  res <- cbind(estim, mse, mae, mape, smape, nna)
  return(res)
}
