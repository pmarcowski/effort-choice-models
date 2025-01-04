#' Model Validation Analysis Pipeline
#'
#' @description
#' This script performs model validation and cross-validation analysis for effort-based
#' decision making models. The analysis pipeline includes:
#' - Data preparation and normalization
#' - Monte Carlo cross-validation (MCCV)
#' - Full model parameter estimation
#' - Leave-one-condition-out (LOCO) cross-validation
#' - Results processing and aggregation
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-03-07
#' @copyright (c) 2023 Przemyslaw Marcowski

# Load packages
library(tidyverse)

# Local source
source("R/models.R")
source("R/validate.R")

#' @section Data Preparation:
#' Load and normalize choice data, calculate derived variables

# Data --------------------------------------------------------------------

# Read choice data
d <- read.table("data/processed/choice.txt")

# Prepare data
d$XStar <- (d$X1 + d$X2) / 2
d$EStar <- (d$E1 + d$E2) / 2
d$G <- scales::rescale(d$X2 - d$XStar, c(0, 1))
d$R <- scales::rescale((d$X2 - d$X1) / d$XStar, c(0, 1))
d$D <- scales::rescale(d$E2 - d$EStar, c(0, 1))
d$E <- scales::rescale((d$E2 - d$E1) / d$EStar, c(0, 1))
mX1 <- max(d$X1)
mX2 <- max(d$X2)
mE1 <- max(d$E1)
mE2 <- max(d$E2)
d$X1 <- d$X1 / mX2
d$X2 <- d$X2 / mX2
d$E1 <- d$E1 / mE2
d$E2 <- d$E2 / mE2
sum(is.na(d)) # check for NAs

# Save prepared choice data
write.table(d, "data/processed/choice_prepared.txt")

# Restructure data
dl <- split(d, d$id, drop = TRUE) # split data by participants

# Reorder splits
dl <- dl[order(names(dl))]

# Model Cross-Validation --------------------------------------------------

# Create vector of model names to run
ms <- c("mBASE", "mHRST", "mHYPER", "mSENS", "mPOWER", "mSIGM", "mDEXPO", "mDPOWER")

# Create vector with number of model parameters
nps <- c(1, 5, 3, 3, 3, 3, 4, 6)
names(nps) <- ms # add names to parameter numbers

# Create list of parameter constraints for each model
lims <- lapply(paste0(ms, "_lim"), get)
names(lims) <- ms # add names to parameter constraints

## Cross-validation ----

# Define function to perform model evaluation
run <- function(nm, d) {
  # Evaluates a model for data set nrep times.
  # Trains model using training data. Validates model using test data.
  # Models are evaluated based on the best of nfit fit attempts.
  #
  # Args:
  #   d: Data
  #
  # Returns:
  #   Number of training and test set data points.
  #   Training log-likelihoods.
  #   Test set loss functions values for each repetition.
  #   Number of estimated model parameters. Parameter estimate values.
  #   Returned values are combined by columns.
  nrep <- nreps
  nfit <- nfits
  prop <- split_prop
  npar <- rep(NA, nrep)
  info <- matrix(NA, ncol = 3, nrow = nrep)
  results <- matrix(NA, ncol = 5, nrow = nrep)
  pars <- matrix(NA, ncol = max(nps), nrow = nrep)

  # Perform validation for nrep repetitions
  for (i in 1:nrep) {
    dsets <- split_data_mccv(d[[nm]], prop)
    info[i, ] <- cbind(nm, nrow(dsets[[1]]), nrow(dsets[[2]]))
    fit <- get_fit_optim(m, nps[m], dsets[[1]], nfit)
    npar[i] <- length(fit[-1])
    results[i, ] <- get_results_discrete(dsets[[2]], m, fit)
    pars[i, c(1:npar[i])] <- fit[-1]
  }

  res <- cbind(info, results, npar, pars)

  return(res)
}

# Set model cross-validations options
nreps <- 100 # num MCCV repetitions
nfits <- 10 # num refits per MCCV repetition
split_prop <- 0.8 # split proportion of train/test data
starts <- list() # create a list to contain par start values

# Run model evaluation for all models ms
for (m in ms) {
  t <- proc.time()[3]
  cat("\n# Model: ", m, sep = "")

  # Obtain set of possible parameter start values for model m
  cat("\n## Initialize parameters...")
  starts[[m]] <- get_starts(m, nps[m], dl)
  write.table(starts[[m]], paste0("output/models/starts/", "cv-", m, ".txt"))
  cat("DONE!")

  # Execute run function for model m
  cat("\n## Validate model...")
  res <- lapply(names(dl), run, dl)
  res <- do.call(rbind, res)
  cat("DONE!")

  # Save results
  write.table(res, paste0("output/models/base/", "cv-", m, ".txt"), col.names = FALSE)
  cat("\n", round(proc.time()[3] - t, 0), " sec elapsed\n", sep = "")
}

# Model Fitting -----------------------------------------------------------

# Set model fitting options
nreps <- 1 # num repetitions
nfits <- 10 # num refits per repetition
split_prop <- 1 # split proportion of train/test data
starts <- list() # create a list to contain par start values

# Run model evaluation for all models ms
for (m in ms) {
  t <- proc.time()[3]
  cat("\n# Model: ", m, sep = "")

  # Obtain set of possible parameter start values for model m
  cat("\n## Initialize parameters...")
  starts[[m]] <- get_starts(m, nps[m], dl)
  write.table(starts[[m]], paste0("output/models/starts/", "fit-", m, ".txt"))
  cat("DONE!")

  # Execute run function for model m
  cat("\n## Fit model...")
  res <- lapply(names(dl), run, dl)
  res <- do.call(rbind, res)
  cat("DONE!")

  # Save results
  write.table(res, paste0("output/models/base/", "fit-", m, ".txt"), col.names = FALSE)
  cat("\n", round(proc.time()[3] - t, 0), " sec elapsed\n", sep = "")
}

# LOCO Analysis -----------------------------------------------------------

# Define function to perform model evaluation with leave-one-condition-out method
run_loco <- function(nm, d) {
  # Evaluates a model for data set nrep times.
  # Trains model using training data. Validates model using test data.
  # Models are evaluated based on the best of nfit fit attempts.
  #
  # Args:
  #   d: Data
  #
  # Returns:
  #   Number of training and test set data points.
  #   Training log-likelihoods.
  #   Test set loss functions values for each repetition.
  #   Number of estimated model parameters. Parameter estimate values.
  #   Returned values are combined by columns.
  nrep <- nreps
  nfit <- nfits
  cond <- condition_name
  npar <- rep(NA, nrep)
  info <- matrix(NA, ncol = 4, nrow = nrep)
  results <- matrix(NA, ncol = 5, nrow = nrep)
  pars <- matrix(NA, ncol = max(nps), nrow = nrep)

  # Perform validation for nrep repetitions
  for (i in 1:nrep) {
    dsets <- split_data_group(d[[nm]], cond, nrep)
    info[i, ] <- cbind(nm, nrow(dsets[[1]]), nrow(dsets[[2]]), dsets[[3]])
    fit <- get_fit_optim(m, nps[m], dsets[[1]], nfit)
    npar[i] <- length(fit[-1])
    results[i, ] <- get_results_discrete(dsets[[2]], m, fit)
    pars[i, c(1:npar[i])] <- fit[-1]
  }

  res <- cbind(info, results, npar, pars)

  return(res)
}

# Set model fitting options
nreps <- 5 # num validation repetitions
nfits <- 10 # num refits per repetition
condition_name <- "E2" # name of condition column to split on
starts <- list() # create a list to contain par start values

# Run model evaluation for all models ms
for (m in ms) {
  t <- proc.time()[3]
  cat("\n# Model: ", m, sep = "")

  # Obtain set of possible parameter start values for model m
  cat("\n## Initialize parameters...")
  starts[[m]] <- get_starts(m, nps[m], dl)
  write.table(starts[[m]], paste0("output/models/starts/", "cv-loco-", m, ".txt"))
  cat("DONE!")

  # Execute run function for model m
  cat("\n## Validate model (LOCO)...")
  res <- lapply(names(dl), run_loco, dl)
  res <- do.call(rbind, res)
  cat("DONE!")

  # Save results
  write.table(res, paste0("output/models/base/", "cv-loco-", m, ".txt"), col.names = FALSE)
  cat("\n", round(proc.time()[3] - t, 0), " sec elapsed\n", sep = "")
}

# Results Processing ------------------------------------------------------

# Combine cross-validation results
cv <-
  data.frame(file_name = fs::dir_ls("output/models/base/", type = "file", regexp = "cv-")) %>%
  mutate(file_content = map(file_name, ~ read.table(paste0(.), sep = " ", row.names = NULL))) %>%
  unnest(cols = c(file_content), keep_empty = TRUE) %>%
  set_names(c(
    "model", "run", "id", "ntrain", "ntest",
    "estim", "logloss", "z_o", "extr", "nna",
    "npar", sprintf("par%i", 1:max(nps))
  )) %>%
  left_join(unique(d[, c("id", "temp_ori", "task_type")])) %>%
  mutate(
    model = str_extract(model, "(?<=-\\w{1})\\w+(?=.txt)"),
    K = ifelse(model == "BASE", npar, npar - 1)
  ) %>%
  group_by(id, model) %>%
  mutate(repn = 1:n()) %>%
  ungroup() %>%
  select(run, id, temp_ori, task_type, model, K, repn, everything()) %>%
  arrange(id)

# Save cross-validation results
write.table(cv, "output/models/processed/cv.txt")

# Combine model-fitting results
fits <-
  data.frame(file_name = fs::dir_ls("output/models/base/", type = "file", regexp = "fit-")) %>%
  mutate(file_content = map(file_name, ~ read.table(paste0(.), sep = " ", row.names = NULL))) %>%
  unnest(cols = c(file_content), keep_empty = TRUE) %>%
  set_names(c(
    "model", "run", "id", "ntrain", "ntest",
    "estim", "logloss", "z_o", "extr", "nna",
    "npar", sprintf("par%i", 1:max(nps))
  )) %>%
  left_join(unique(d[, c("id", "temp_ori", "task_type")])) %>%
  mutate(
    model = str_extract(model, "(?<=-\\w{1})\\w+(?=.txt)"),
    K = ifelse(model == "BASE", npar, npar - 1)
  ) %>%
  group_by(id, model) %>%
  mutate(repn = 1:n()) %>%
  ungroup() %>%
  select(run, id, temp_ori, task_type, model, K, repn, everything()) %>%
  arrange(id)

# Combine LOCO results
loco <-
  data.frame(file_name = fs::dir_ls("output/models/base/", type = "file", regexp = "cv-loco-")) %>%
  mutate(file_content = map(file_name, ~ read.table(paste0(.), sep = " ", row.names = NULL))) %>%
  unnest(cols = c(file_content), keep_empty = TRUE) %>%
  set_names(c(
    "model", "run", "id", "ntrain", "ntest", "fold",
    "estim", "logloss", "z_o", "extr", "nna",
    "npar", sprintf("par%i", 1:max(nps))
  )) %>%
  left_join(unique(d[, c("id", "temp_ori", "task_type")])) %>%
  mutate(
    model = str_extract(model, "(?<=-\\w{1})\\w+(?=.txt)"),
    K = ifelse(model == "BASE", npar, npar - 1)
  ) %>%
  group_by(id, model) %>%
  mutate(repn = 1:n()) %>%
  ungroup() %>%
  select(run, id, temp_ori, task_type, model, K, repn, everything()) %>%
  arrange(id)

# Save LOCO results
write.table(loco, "output/models/processed/loco.txt")
