# Title: Toolbox of helper functions for effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-05-12
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

count_changepoints <- function(x) {
  # Counts the number of changepoints in a numeric vector.
  # A changepoint is a position at which the vector changes from increasing to decreasing, or vice versa.
  #
  # Args:
  #   x: Numeric vector
  #
  # Returns:
  #   The number of changepoints in the input vector.

  n <- length(x)
  if (n < 2) {
    stop("'x' must be a vector of length greater than 1")
  }

  # Initialize the count of changepoints and the current sequence
  changepoints <- 0

  # Define current_sequence based on the first two elements
  if (x[2] > x[1]) {
    current_sequence <- "increasing"
  } else if (x[2] < x[1]) {
    current_sequence <- "decreasing"
  } else {
    current_sequence <- "equal"
  }

  # Iterate over each element in the vector starting from the third one
  for (i in 3:n) {
    # If the current number is greater than the previous one and the current sequence is not "increasing"
    if (x[i] > x[i - 1] && current_sequence != "increasing") {
      current_sequence <- "increasing"
      changepoints <- changepoints + 1
    }

    # If the current number is less than the previous one and the current sequence is not "decreasing"
    else if (x[i] < x[i - 1] && current_sequence != "decreasing") {
      current_sequence <- "decreasing"
      changepoints <- changepoints + 1
    }
  }

  return(changepoints)
}

detect_sequences <- function(x) {
  # Takes a numeric vector and detects strictly increasing,
  # strictly decreasing, and equal sequences. It returns a character string
  # that corresponds to the order of these sequences, i.e., "increasing-decreasing"
  # if the vector first increases and then decreases; "increasing" if it simply increases;
  # "decreasing" if it simply decreases; or "equal" if all elements are the same.
  #
  # Args:
  #   x: A numeric vector.
  #
  # Returns:
  #   Character string that describes the sequence of the vector ("increasing",
  #   "decreasing", "increasing-decreasing", "decreasing-increasing", or "equal").

  if (length(x) < 2) {
    return(NA)
  } # Handle short vectors

  # Initialize empty vector for sequence
  sequence <- c()

  # Iterate over each element in vector
  for (i in 2:length(x)) {
    # Check for increasing sequence
    if (x[i] > x[i - 1]) {
      sequence <- c(sequence, "increasing")
    }

    # Check for decreasing sequence
    else if (x[i] < x[i - 1]) {
      sequence <- c(sequence, "decreasing")
    }

    # Check for equal sequence
    else {
      sequence <- c(sequence, "equal")
    }
  }

  # Remove consecutive duplicates using run length encoding
  sequence <- rle(sequence)$values

  # Initialize result
  result <- ""

  # Determine result based on sequence patterns
  if (all(sequence == "increasing")) {
    result <- "increasing"
  } else if (all(sequence == "decreasing")) {
    result <- "decreasing"
  } else if (all(sequence == "equal")) {
    result <- "equal"
  } else if (length(sequence) > 1) {
    if (sequence[1] == "increasing" && tail(sequence, 1) == "decreasing") {
      result <- "increasing-decreasing"
    } else if (sequence[1] == "decreasing" && tail(sequence, 1) == "increasing") {
      result <- "decreasing-increasing"
    }
  }

  return(result)
}

simulate_agent_pest <- function(conditions, outcomes, pest_steps, agent_pars, choice_model) {
  # Simulates agent choices for PEST procedure
  #
  # Args:
  #   conditions: Numeric vector of conditions for all choice scenarios
  #   outcomes: Numeric vector of min and max payoffs for simulation
  #   pest_steps: Numeric vector of initial, minimum, and maximum adjustment step values
  #   agent_pars: List of parameter sets per agent to simulate
  #   choice_model: Model function returning choices
  #
  # Returns:
  #   A data frame containing simulated choices for all agents

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

# Define function to offset and log-transform parameters
offset_log <- function(x) {
  # Applies offset (to avoid negative values) and log-transformation
  offset <- abs(min(x)) + 1e-7
  y <- x + offset
  return(log(y))
}

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

# Power analysis ----------------------------------------------------------

calculate_power <- function(effect, log_odds_ratio) {
  # Calculates power for a given effect and log odds ratio.
  #
  # Args:
  #   effect: The effect to calculate power for.
  #   log_odds_ratio: The log odds ratio to use for power calculation
  #
  # Returns:
  #   A list containing odds ratio, probability difference, and power.

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
