#' Helper Functions for Analysis Pipeline
#'
#' @description
#' Collection of utility functions supporting the analysis pipeline.
#' Functions include:
#' - Pattern detection and change point analysis
#' - Agent simulation for PEST procedure
#' - Parameter transformation and normalization
#' - Power analysis and visualization helpers
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-05-12
#' @copyright (c) 2023 Przemyslaw Marcowski

#' Analyze Value Function Sequences
#'
#' Helper function that analyzes sequence patterns and changepoints.
#' @param x Numeric vector
#' @param tol Numeric tolerance for equality comparison (default: sqrt(.Machine$double.eps))
#' @return List containing sequence patterns and changepoint count
get_sequence_patterns <- function(x, tol = sqrt(.Machine$double.eps)) {
  # Input validation
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  if (length(x) < 2) return(list(sequences = NA_character_, changepoints = 0))
  
  n <- length(x)
  sequences <- character(n - 1)
  changepoints <- 0
  
  # Find first non-equal sequence using tolerance
  i <- 1
  while (i < n && abs(x[i + 1] - x[i]) < tol) {
    sequences[i] <- "equal"
    i <- i + 1
  }
  
  # Handle edge case where all values are equal
  if (i == n) {
    return(list(
      sequences = "equal",
      changepoints = 0
    ))
  }
  
  # Initialize sequence based on first non-equal pair
  current_sequence <- if (x[i + 1] > x[i]) "increasing" else "decreasing"
  sequences[i] <- current_sequence
  
  # Analyze remaining elements
  for (j in (i + 2):n) {
    diff <- x[j] - x[j - 1]
    
    if (abs(diff) < tol) {
      sequences[j - 1] <- "equal"
      next
    }
    
    new_sequence <- if (diff > 0) "increasing" else "decreasing"
    sequences[j - 1] <- new_sequence
    
    if (new_sequence != current_sequence) {
      changepoints <- changepoints + 1
      current_sequence <- new_sequence
    }
  }
  
  # Clean up sequences by removing consecutive duplicates
  clean_sequences <- rle(sequences)$values
  
  return(list(
    sequences = clean_sequences,
    changepoints = changepoints
  ))
}

#' Detect Value Function Pattern Type
#'
#' Analyzes sequence patterns in value functions to classify their shape.
#'
#' @param x Numeric vector of values
#' @return Character string: "increasing", "decreasing", "increasing-decreasing",
#'         "decreasing-increasing", or "equal"
get_sequence_profile <- function(x) {
  result <- get_sequence_patterns(x)
  if (is.na(result$sequences[1])) return(NA_character_)
  
  # Remove any "equal" sequences for pattern determination
  sequence <- result$sequences[result$sequences != "equal"]
  
  # Handle case where all values were equal
  if (length(sequence) == 0) return("equal")
  
  # Handle single direction cases
  if (length(sequence) == 1) {
    return(sequence)
  }
  
  # Handle two-direction cases
  if (length(sequence) == 2) {
    if (sequence[1] == "increasing" && sequence[2] == "decreasing") {
      return("increasing-decreasing")
    }
    if (sequence[1] == "decreasing" && sequence[2] == "increasing") {
      return("decreasing-increasing")
    }
  }
  
  # If we can't categorize into one of our defined patterns, return NA
  return(NA_character_)
}

#' Simulate Agent Choices Using PEST Procedure
#'
#' Simulates choices for multiple agents using Parameter Estimation by
#' Sequential Testing (PEST) procedure.
#'
#' @param conditions Vector of effort conditions
#' @param outcomes Min/max payoffs
#' @param pest_steps PEST adjustment steps
#' @param agent_pars List of agent parameters
#' @param choice_model Model function for choice generation
#' @return Data frame of simulated choices
simulate_agent_pest <- function(conditions, outcomes, pest_steps, agent_pars, choice_model) {
  # Create list to store choices for each agent
  agent_choices <- vector("list", length(agent_pars))
  names(agent_choices) <- names(agent_pars)

  # Iterate over each agent
  for (agent_id in names(agent_pars)) {
    # Randomize condition order for current agent
    condition_order <- sample(seq_along(conditions))

    # Apply simulation for each condition for current agent
    agent_choices[[agent_id]] <- lapply(seq_along(condition_order), function(trial) {
      # Get current condition index
      condition_index <- condition_order[trial]

      # Initialize variables for simulation
      consecutive_same_choices <- 0
      adjustment_step <- pest_steps[1]
      trial_choices <- matrix(NA, nrow = 0, ncol = 9)
      prev_step_doubled <- FALSE
      had_choice_switch <- FALSE
      choice <- NA # Initialize choice variable

      # Continue simulation while adjustment step is within specified range
      while (adjustment_step >= pest_steps[2] & adjustment_step <= pest_steps[3]) {
        # Set up initial effort and option values for first choice
        if (nrow(trial_choices) == 0) {
          effortless_effort <- 0
          effortless_reward <- outcomes[2] / 2
          effortful_effort <- conditions[condition_index]
          effortful_reward <- outcomes[2]
        } else {
          # Update adjustment step based on previous choice
          if (!is.na(choice) && choice == trial_choices[nrow(trial_choices), 9]) {
            consecutive_same_choices <- consecutive_same_choices + 1
            if (consecutive_same_choices >= 4) {
              adjustment_step <- adjustment_step * 2
              prev_step_doubled <- TRUE
            } else if (consecutive_same_choices == 3 & !prev_step_doubled & had_choice_switch) {
              adjustment_step <- adjustment_step * 2
              prev_step_doubled <- TRUE
            }
          } else {
            consecutive_same_choices <- 0
            adjustment_step <- adjustment_step / 2
            prev_step_doubled <- FALSE
            had_choice_switch <- TRUE
          }

          # Update effortless_reward based on previous choice
          if (!is.na(choice) && choice == 1) {
            effortless_reward <- trial_choices[nrow(trial_choices), 2] + adjustment_step
          } else if (!is.na(choice)) {
            effortless_reward <- trial_choices[nrow(trial_choices), 2] - adjustment_step
          }
        }

        # Make prediction using specified choice model
        pred <- do.call(choice_model, list(as.numeric(agent_pars[[agent_id]][, -1]),
          list(
            X1 = effortless_reward,
            E1 = effortless_effort,
            X2 = effortful_reward,
            E2 = effortful_effort
          ),
          choice = TRUE
        ))
        subjective_values <- as.numeric(pred[-1])
        prob_effortful <- as.numeric(pred[1])

        # Ensure prob_effortful is a valid probability
        if (is.na(prob_effortful) || prob_effortful < 0 || prob_effortful > 1) {
          prob_effortful <- 0.5 # Default to 0.5 if invalid
        }

        # Simulate choice based on predicted probability
        choice <- rbinom(1, 1, prob_effortful)

        # Store choice and related information in trial_choices matrix
        trial_choices <- rbind(trial_choices, c(
          trial, effortless_reward, effortless_effort,
          effortful_reward, effortful_effort, subjective_values,
          prob_effortful, choice
        ))
      }

      # Return trial_choices matrix as data frame
      return(as.data.frame(trial_choices))
    })
  }

  # Combine choices from all agents into single data frame
  combined_choices <- do.call(rbind, lapply(agent_choices, function(agent_data) {
    do.call(rbind, agent_data)
  }))
  combined_choices <- as.data.frame(combined_choices)

  # Add agent IDs as new column in combined choices data frame
  combined_choices <- cbind(
    rep(
      names(agent_choices),
      sapply(agent_choices, function(x) {
        sum(sapply(x, nrow))
      })
    ),
    combined_choices
  )

  # Set column names for combined choices data frame
  colnames(combined_choices) <- c(
    "id", "trial", "X1", "E1", "X2", "E2", "sv1",
    "sv2", "EffortfulOptionChosenProb",
    "EffortfulOptionChosen"
  )

  # Return combined choices data frame
  return(combined_choices)
}

#' Log Transform Parameters with Offset
#'
#' @param x Numeric vector
#' @return Log-transformed values with offset to avoid negative values
offset_log <- function(x) {
  # Applies offset (to avoid negative values) and log-transformation
  offset <- abs(min(x)) + 1e-7
  y <- x + offset
  return(log(y))
}

#' Generate Random Proportions Matrix
#'
#' @param ncol Number of columns (proportions)
#' @param nrow Number of rows
#' @param var.names Optional column names
#' @return Data frame of random proportions summing to 1
random_props <- function(ncol, nrow, var.names = NULL) {
  # Generates random proportions
  if (ncol < 2) stop("ncol must be greater than 1")
  p <- function(n) {
    y <- 0
    z <- sapply(seq_len(n - 1), function(i) {
      x <- sample(seq(0, 1 - y, by = .01), 1)
      y <<- y + x
      return(x)
    })
    w <- c(z, 1 - sum(z))
    return(w)
  }
  DF <- data.frame(t(replicate(nrow, p(n = ncol))))
  if (!is.null(var.names)) colnames(DF) <- var.names

  return(DF)
}

#' Calculate Statistical Power for Model Effects
#'
#' @param effect Effect name to analyze
#' @param log_odds_ratio Effect size as log odds ratio
#' @return List with odds ratio, probability difference, and power
calculate_power <- function(effect, log_odds_ratio) {
  # Get coefficient name and original values from model
  coef_names <- rownames(summary(choice_mod)$coefficients$cond)

  if (grepl(":", effect)) { # interaction effect
    coef_name <- coef_names[grep(gsub(":", ".*:", effect), coef_names)]
  } else { # main effect
    coef_name <- coef_names[grep(paste0("^", effect), coef_names)]
  }

  original_se <- summary(choice_mod)$coefficients$cond[coef_name, "Std. Error"]

  # Calculate z-score and power
  z <- log_odds_ratio / original_se
  power <- as.numeric(pnorm(abs(z) - qnorm(0.975)) + pnorm(-abs(z) - qnorm(0.975)))

  # Calculate probability difference
  baseline_prob <- 0.5
  new_prob <- plogis(qlogis(baseline_prob) + log_odds_ratio)
  prob_diff <- new_prob - baseline_prob

  return(list(odds_ratio = exp(log_odds_ratio), prob_diff = prob_diff, power = power[1]))
}

#' Normalize Parameters to Specified Ranges
#'
#' @param x Data frame with parameters
#' @param norm_pars List of normalization ranges
#' @return Data frame with normalized parameters
normalize_param <- function(x, norm_pars) {
  # Conditionally normalizes parameter columns to specified ranges

  # Find common columns for applying normalization
  par_norm <- intersect(names(norm_pars), colnames(x))

  # Apply normalization to each column
  for (colname in par_norm) {
    x[[colname]] <- scales::rescale(x[[colname]], norm_pars[[colname]])
  }

  # Return normalized dataframe
  return(x)
}

#' Generate Split Legend Key for Plots
#'
#' @param cols Vector of colors
#' @return grob object for composite legend key
split_legend_key <- function(cols) {
  # Generates split legend key
  grid::polygonGrob(
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1),
    id = rep(1:2, each = 2),
    gp = grid::gpar(
      fill = cols,
      col = "black"
    )
  )
}
