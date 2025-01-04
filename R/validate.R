#' Model Validation Utilities
#'
#' @description
#' Collection of functions for model validation and evaluation:
#' - Data splitting functions for cross-validation
#' - Parameter initialization and optimization
#' - Model fitting with different algorithms
#' - Model evaluation metrics
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-05-12
#' @copyright (c) 2023 Przemyslaw Marcowski

#' Split Data for Monte Carlo Cross-Validation
#'
#' Subsamples data into training and test sets according to proportion p.
#'
#' @param d Data to split
#' @param p Proportion for splitting into training/test sets
#' @return List containing training and test sets
split_data_mccv <- function(d, p) {
  # If p < 1, perform train/test split
  if (p < 1) {
    nchos <- nrow(d)
    nfset <- floor(nchos * p)  # Calculate training set size
    fset <- sample(1:nchos, nfset)  # Randomly sample indices
    return(list(d[fset, ], d[(1:nchos)[!(1:nchos) %in% fset], ]))
  } else {
    # If p = 1, use all data for both sets
    return(list(d, d))
  }
}

#' Split Data for K-Fold Cross-Validation
#'
#' Splits data into k folds for cross-validation.
#'
#' @param d Data to split
#' @param k Number of folds
#' @param r Current validation repetition
#' @param shuffle Whether to reshuffle data on loop start
#' @return List containing training and test sets
split_data_kfold <- function(d, k, r, shuffle = TRUE) {
  # Handle special cases
  if (k == 1) return(list(d, d))

  # Reshuffle data at start of each k-fold cycle if requested
  else if (shuffle == TRUE) if (r %% k == 1) d <- d[sample(nrow(d)), ]

  # Split data into k equal-sized folds
  nchos <- nrow(d)
  nfset <- rep(1:k, diff(floor(nchos * seq(0, 1, length.out = k + 1))))
  fset <- split(d, nfset)

  # Return training set (k-1 folds) and test set (1 fold)
  return(list(do.call(rbind, fset[-((r %% k) + 1)]), fset[[(r %% k) + 1]]))
}

#' Split Data for Nested K-Fold Cross-Validation
#'
#' Creates nested k-fold split for hyperparameter tuning.
#'
#' @param d Data to split
#' @param k_outer Number of outer folds
#' @param k_inner Number of inner folds
#' @param r_outer Current outer repetition
#' @param r_inner Current inner repetition
#' @param shuffle Whether to reshuffle data
#' @return List containing training, validation and test sets
split_data_nested_kfold <- function(d, k_outer, k_inner, r_outer, r_inner, shuffle = TRUE) {
  # Handle special case of no folding
  if (k_outer == 1 && k_inner == 1) return(list(d, d))

  # Reshuffle data at start of outer fold cycle if requested
  else if (shuffle == TRUE) {
    if (r_outer %% k_outer == 1) d <- d[sample(nrow(d)), ]
  }

  # Create outer folds
  nchos_outer <- nrow(d)
  nfset_outer <- rep(1:k_outer, diff(floor(nchos_outer * seq(0, 1, length.out = k_outer + 1))))
  fset_outer <- split(d, nfset_outer)

  # Split into outer training and test sets
  train_data <- fset_outer[-(r_outer %% k_outer + 1)]
  test_data <- fset_outer[[(r_outer %% k_outer + 1)]]

  # Reshuffle training data at start of inner fold cycle if requested
  if (shuffle == TRUE) {
    if (r_inner %% k_inner == 1) train_data <- train_data[sample(nrow(train_data)), ]
  }

  # Create inner folds from training data
  nchos_inner <- nrow(train_data)
  nfset_inner <- rep(1:k_inner, diff(floor(nchos_inner * seq(0, 1, length.out = k_inner + 1))))
  fset_inner <- split(train_data, nfset_inner)

  # Split into inner training and validation sets
  train_data_inner <- fset_inner[-(r_inner %% k_inner + 1)]
  valid_data_inner <- fset_inner[[(r_inner %% k_inner + 1)]]

  return(list(train_data_inner, valid_data_inner, test_data))
}

#' Resample Data with Replacement
#'
#' Resamples dataset with replacement.
#'
#' @param d Data to resample
#' @param v If TRUE, model is validated using OOB data. If FALSE, original data is used.
#' @return List containing training and test sets
split_data_boot <- function(d, v = TRUE) {
  nchos <- nrow(d)
  fset <- sample(1:nchos, replace = TRUE)
  if (v == TRUE) {
    return(list(d[fset, ], d[(1:nchos)[!(1:nchos) %in% fset], ]))
  } else {
    return(list(d[fset, ], d))
  }
}

#' Split Data Based on Grouping Condition
#'
#' Splits data into training and test set based on grouping condition.
#'
#' @param d Data to split
#' @param group_col Name of grouping column in d
#' @param r Current validation repetition
#' @return List containing training and test sets
split_data_group <- function(d, group_col, r) {
  # Get unique groups
  groups <- unique(d[[group_col]])

  # Handle special case of single group
  if (length(groups) == 1) return(list(train = d, test = d))

  # Select test group in rotation based on repetition number
  test_group <- groups[((r - 1) %% length(groups)) + 1]
  is_test <- d[[group_col]] == test_group

  # Split data based on selected test group
  return(list(d[!is_test, ], d[is_test, ], test_group))
}

#' Wrapper Function for Data Splitting Methods
#'
#' Splits data according to selected method.
#'
#' @param d Data to split
#' @param method Splitting method, one of: "MCCV", "kfold", "boot", "group"
#' @param ... Additional arguments passed to specific splitting function
#' @return List containing training and test sets based on chosen method
split_data <- function(d, method, ...) {
  if (method == "MCCV") {
    dout <- split_data_MCCV(d, ...)
  } else if (method == "kfold") {
    dout <- split_data_kfold(d, ...)
  } else if (method == "boot") {
    dout <- split_data_boot(d, ...)
  } else if (method == "condition") {
    dout <- split_data_condition(d, ...)
  } else {
    stop("'method' can only be 'mccv', 'kfold', 'boot', or 'condition'")
  }
  return(dout)
}

#' Draw Parameter Starting Values
#'
#' @param x Model string identifier
#' @return Vector of starting parameter values
draw_starts <- function(x) {
  # Initialize empty start vector
  start <- c()

  # Sample one value from each column
  for (i in 1:ncol(x)) start[i] <- sample(x[, i], 1)
  return(start)
}

#' Generate Random Starting Values
#'
#' @param m Model string identifier
#' @return Vector of randomized parameter values within bounds
rand_starts <- function(m) {
  # Initialize empty start vector
  start <- c()
  lim <- lims[[m]]

  # Generate random values within bounds for each parameter
  for (i in 1:ncol(lim)) {
    start[i] <- runif(
      1,
      ifelse(lim[1, i] < -1e+10, -1e+10, lim[1, i]),
      ifelse(lim[2, i] > 1e+10, 1e+10, lim[2, i])
    )
  }
  return(start)
}

#' Get Starting Values for Model Parameters
#'
#' Fits a model to data once and saves parameter estimates to drive.
#'
#' @param m Model string for which parameters are to be estimated
#' @param np Number of model parameters
#' @param dx Data for fitting
#' @param ... Additional arguments passed to inner functions
#' @return Parameter values. Parameter estimates are also saved to drive.
get_starts <- function(m, np, dx, ...) {
  # Initialize parameters list
  par <- lapply(dx, function(x) {
    run <- 0
    fit <- FALSE
    starts <- rep(1, np)  # Default starting values

    # Keep trying until successful fit
    while (fit == FALSE) {
      run <- run + 1
      # Try optimization with default or random starts
      res_tmp <- try(if (run == 1) {
        # First try with default starts
        optim(
          starts, get(m),
          d = x, ...,
          method = ifelse(np > 1, "L-BFGS-B", "Brent"),
          lower = lims[[m]][1, ], upper = lims[[m]][2, ]
        )
      } else {
        # Then try with random starts
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

  # Combine results
  pars <- do.call(rbind, par)
  return(pars)
}

#' Fit Model Using L-BFGS-B or Brent
#'
#' Fits model using L-BFGS-B (or Brent for one-dimensional models).
#' Fitting is performed by estimator MINIMIZATION.
#' Fitting is repeated until nfit successful fits are obtained.
#' Uses regularization if epsilon is supplied.
#'
#' @param m Model string for model to be fitted
#' @param np Number of model parameters
#' @param dfit Data for fitting
#' @param nfit Number of fits performed to get best fit
#' @param ... Additional arguments passed to inner functions
#' @return Estimator value resulting from best fit. Parameter values resulting from best fit.
get_fit_optim <- function(m, np, dfit, nfit, ...) {
  # Initialize tracking variables
  fit_v <- Inf
  fit_c <- 0

  # Keep trying until desired number of successful fits
  while (fit_c < nfit) {
    # Attempt optimization
    res_tmp <- try(
      optim(
        draw_starts(starts[[m]]), get(m),
        d = dfit, ...,
        method = ifelse(np > 1, "L-BFGS-B", "Brent"),
        lower = lims[[m]][1, ], upper = lims[[m]][2, ]
      ), silent = FALSE)

    # Track successful fits and keep best result
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

  # Return results if valid fit found
  if (fit_v != -Inf) {
    return(c(fit_v, fit_p))
  } else {
    return(NULL)
  }
}

#' Fit Model Using Differential Evolution Algorithm
#'
#' Fits model using differential evolution algorithm.
#' Fitting is performed by estimator MINIMIZATION.
#' Fitting is repeated until nfit successful fits are obtained.
#' Uses regularization if epsilon is defined.
#' Pre-defined number of algorithm iterations niters is required.
#' Dependencies: DEoptim
#'
#' @param dfit Data for fitting
#' @param m Model string for model to be fitted
#' @param nfit Number of fits performed to get best fit
#' @param ... Additional arguments passed to inner functions
#' @return Estimator value resulting from best fit. Parameter values resulting from best fit.
get_fit_de <- function(dfit, m, nfit, ...) {
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

#' Fit Model Using Genetic Algorithm
#'
#' Fits model using a genetic algorithm.
#' Fitting is performed by estimator MAXIMIZATION.
#' Fitting is repeated until nfit successful fits are obtained.
#' Uses regularization if epsilon is defined.
#' Pre-defined number of algorithm populations npops is required.
#' Pre-defined number of algorithm runs nruns is required.
#' Pre-defined number of algorithm iterations niters is required.
#' Dependencies: GA
#'
#' @param dfit Data for fitting
#' @param m Model string for model to be fitted
#' @param nfit Number of fits performed to get best fit
#' @param ... Additional arguments passed to inner functions
#' @return Estimator value resulting from best fit. Parameter values resulting from best fit.
get_fit_ga <- function(dfit, m, nfit, ...) {
  # Load required package
  require(GA)

  # Initialize tracking variables
  fit_v <- -Inf
  fit_c <- 0

  # Keep trying until desired number of successful fits
  while (fit_c < nfit) {
    # Attempt genetic algorithm optimization
    res_tmp <- try(ga(
      type = "real-valued", fitness = get(m), d = dfit, ...,
      lower = lims[[m]][1, ], upper = lims[[m]][2, ],
      suggestions = draw_starts(starts[[m]]), get(m),
      popSize = npops, run = nruns, maxiter = niters
    ),
    silent = FALSE
    )

    # Track successful fits and keep best result
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

  # Return results if valid fit found
  if (fit_v != Inf) {
    return(c(fit_v, fit_p))
  } else {
    return(NULL)
  }
}

#' Fit Model Using Specified Method
#'
#' Fits model using specified method.
#'
#' @param d Data for fitting
#' @param method Fitting method, one of: "optim", "de", "ga"
#' @param m Model string for model to be fitted
#' @param ... Additional arguments passed to inner functions
#' @return Estimator value resulting from best fit. Parameter values resulting from best fit.
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

#' Extract Model Training and Test Results for Classification Problems
#'
#' Extracts model training and test results in classification problems.
#'
#' @param dpre Test data set
#' @param m Model string for model to be evaluated
#' @param fit Model fit
#' @param ... Additional arguments passed to inner functions
#' @return Training model log-likelihood. Multiple loss functions values obtained based on test data set. Extremity abs(pred - .5). Number of NAs obtained during modeling.
get_results_discrete <- function(dpre, m, fit, ...) {
  # Extract fit estimator
  estim <- fit[1]

  # Calculate predictions
  pred <- eval(call(m, quote(fit[-1]), quote(dpre), choice = TRUE, quote(...)))

  # Calculate performance metrics
  logloss <- (1 / nrow(dpre)) * -sum(log(ifelse(dpre$EffortfulOptionChosen == 1, pred[, 1], 1 - pred[, 1])))
  z_o <- mean(abs(pred[, 1] - dpre$EffortfulOptionChosen), na.rm = TRUE)
  extr <- mean(abs(pred[, 1] - .5), na.rm = TRUE)
  nna <- sum(is.na(pred[, 1]))

  # Combine results
  res <- cbind(estim, logloss, z_o, extr, nna)
  return(res)
}

#' Extract Model Training and Test Results for Regression Problems
#'
#' Extracts model training and test results in regression problems.
#'
#' @param dpre Test data set
#' @param m Model string for model to be evaluated
#' @param fit Model fit
#' @param ... Additional arguments passed to inner functions
#' @return Training model log-likelihood. Multiple loss functions values obtained based on test data set. Extremity abs(pred - .5). Number of NAs obtained during modeling.
get_results_regression <- function(dpre, m, fit, ...) {
  # Extract fit estimator
  estim <- fit[1]

  # Calculate predictions
  pred <- eval(call(m, quote(fit[-1]), quote(dpre), pred = TRUE, quote(...)))

  # Calculate performance metrics
  mse <- mean((dpre$y - pred)^2, na.rm = TRUE)
  mae <- mean(abs(dpre$y - pred), na.rm = TRUE)
  mape <- mean(abs((dpre$y - pred) / dpre$y)) * 100
  smape <- mean(2 * abs(dpre$y - pred) / (abs(dpre$y) + abs(pred)), na.rm = TRUE) * 100
  nna <- sum(is.na(pred))

  # Combine results
  res <- cbind(estim, mse, mae, mape, smape, nna)
  return(res)
}
