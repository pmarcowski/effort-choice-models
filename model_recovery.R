#' Model Recovery Analysis for Effort Choice Models
#'
#' @description
#' This script performs model recovery analysis to validate parameter estimation
#' and behavioral predictions. The analysis pipeline includes:
#' - Simulation of choice data using fitted parameters
#' - Parameter recovery analysis using simulated data
#' - Bland-Altman analysis of parameter recovery
#' - Validation of behavioral effects in simulated agents
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-03-07
#' @copyright (c) 2023 Przemyslaw Marcowski

# Load packages
library(tidyverse)
library(glmmTMB)
library(patchwork)

# Local source
source("R/models.R")
source("R/validate.R")
source("R/helpers.R")
source("R/colors.R")

# Set RNG seed
set.seed(42)

# Plotting options
temp_ori_color <- plot_colors[c(4, 3)] # plotting colors
par_labels <- c(
  "par1" = "delta[1]",
  "par3" = "gamma[1]",
  "par2" = "delta[2]",
  "par4" = "gamma[2]",
  "par5" = "omega"
  )

# Simulation Setup --------------------------------------------------------

# Get values of effort and payoffs from original empirical data for normalization
org <- read.table("data/processed/choice.txt")
mE <- max(org$E2)
mX <- max(org$X2)
mnX <- min(org$X1)

# Set simulation parameters
efforts <- c(45, 75, 115, 175, 270) / mE # normalized effort levels for simulation
payoffs <- c(mnX, 15) / mX # normalized min and max payoffs for simulation
pest_steps <- c(1, 0.1, 30) / mX # normalized pest adjustment steps

# Define name of model to run
m <- "mDPOWER"

# Set number of model parameters
nps <- c(6)
names(nps) <- m

# Create list of parameter constrains for each model
lims <- list(mDPOWER_lim)
names(lims) <- m

## Recover choices ----

# Load prepared empirical choice data
emp <- read.table("data/processed/choice_prepared.txt")
emp <- emp[, c("id", "session", "temp_ori", "task_type", "trial", "X1", "E2", "EffortfulOptionChosen")]

# Retrieve original model parameters
# DPOWER:
#   1: Delta1
#   2: Delta2
#   3: Gamma1
#   4: Gamma2
#   5: Omega
#   6: Softmax
pars <- read.table("output/models/processed/fits.txt")
pars <- pars[pars$model %in% gsub("m", "", m), ]
pars <- pars[, c("id", sprintf("par%i", 1:6))]

# Simulate agents for DPOWER model
agent_pars <- split(pars, pars$id)
sim <- simulate_agent_pest(efforts, payoffs, pest_steps, agent_pars, m)
sim <- sim[, c("id", "trial", "X1", "E1", "X2", "E2", "EffortfulOptionChosen")]
sim <- left_join(sim, unique(emp[, c("id", "session", "temp_ori", "task_type")]))

# Compare distributions of empirical and simulated choices
real_choices <- emp$EffortfulOptionChosen
simulated_choices <- sim$EffortfulOptionChosen
choice_table <- rbind(table(real_choices), table(simulated_choices))
choice_test <- chisq.test(choice_table)
choice_test

## Parameter Recovery ----

# Define function for model fitting
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

# Recover parameters by fitting model to simulated choices
nreps <- 1
nfits <- 10
split_prop <- 1
siml <- split(sim, sim$id)
starts <- list(get_starts(m, nps, siml))
names(starts) <- m
rec <- lapply(names(siml), run, siml)
rec <- as.data.frame(do.call(rbind, rec))
colnames(rec) <- c(
  "id", "ntrain", "ntest",
  "estim", "logloss", "z_o", "extr", "nna",
  "npar", sprintf("par%i", 1:max(nps))
)
rec[, -1] <- sapply(rec[, -1], function(x) as.numeric(x))
rec <- rec[, colnames(rec) %in% c("id", sprintf("par%i", 1:5))]
rec <- rec[match(pars$id, rec$id), ]

# Apply range normalization to each parameter
pars[, -1] <- sapply(pars[, -1], function(x) offset_log(x))
rec[, -1] <- sapply(rec[, -1], function(x) offset_log(x))
pars <- left_join(pars, unique(emp[, c("id", "session", "temp_ori", "task_type")]))
rec <- left_join(rec, unique(emp[, c("id", "session", "temp_ori", "task_type")]))

# Perform permutation test and calculate confidence intervals for each parameter
par_tests <- data.frame(par = character(), diff = numeric(), lower_ci = numeric(), upper_ci = numeric(), p = numeric())
num_perm = 10000
conf_level = 0.95

# Perform permutation test and calculate confidence intervals for each parameter
for (par in sprintf("par%i", 1:5)) {
  x <- pars[[par]]
  y <- rec[[par]]
  diff <- x - y
  obs_diff <- mean(diff)

  comb <- c(x, y)
  perm_diffs <- numeric(num_perm)

  for (i in 1:num_perm) {
    perm <- sample(comb, length(comb), replace = FALSE)
    perm_x <- perm[1:length(x)]
    perm_y <- perm[(length(x) + 1):length(comb)]
    perm_diffs[i] <- mean(perm_x - perm_y)
  }

  p <- mean(abs(perm_diffs) >= abs(obs_diff))

  test <- data.frame(par = par, diff = obs_diff, p = p)
  par_tests <- rbind(par_tests, test)
}

par_tests$par <- recode(par_tests$par, !!!par_labels)
par_tests$diff <- exp(par_tests$diff)
knitr::kable(par_tests, digits = 3)
write.csv(as.data.frame(par_tests), "output/tables/recovered_parameter_tests.csv")

# Calculate Bland-Altman values for each parameter
par_ba <- data.frame(
  par = character(),
  temp_ori = character(),
  task_type = character(),
  mean_value = numeric(),
  diff = numeric(),
  mean_diff = numeric(),
  upper_limit = numeric(),
  lower_limit = numeric()
)

for (par in sprintf("par%i", 1:5)) {
  x <- pars[[par]]
  y <- rec[[par]]

  temp_ori <- pars$temp_ori
  task_type <- pars$task_type

  mean_diff <- mean(x - y)
  sd_diff <- sd(x - y)

  ba <- data.frame(
    par = par,
    temp_ori = temp_ori,
    task_type = task_type,
    mean_value = (x + y) / 2,
    diff = x - y,
    mean_diff = mean_diff,
    upper_limit = mean_diff + 1.96 * sd_diff,
    lower_limit = mean_diff - 1.96 * sd_diff
  )

  par_ba <- rbind(par_ba, ba)
}

# Create Bland-Altman plot

# Create condition labels
par_ba$par <- factor(par_ba$par, levels = names(par_labels), labels = par_labels)
par_ba$cond <- with(par_ba, paste0(task_type, " (", temp_ori, ")"))
legend_lims <- sort(unique(par_ba$cond))
legend_cols <- rep(temp_ori_color, 2)
legend_shapes <- rep(c(1, 2), each = 2)

par_ba_plot <- par_ba %>%
  ggplot(aes(x = mean_value, y = diff, color = cond, shape = cond)) +
  geom_point(alpha = 0.5) +
  geom_hline(aes(yintercept = mean_diff), color = "black", linetype = "dashed") +
  geom_hline(aes(yintercept = upper_limit), color = "red3", linetype = "dashed") +
  geom_hline(aes(yintercept = lower_limit), color = "red3", linetype = "dashed") +
  labs(
    title = "Log-Transformed Bland-Altman Plots for Empirical and Recovered Parameters\n",
    x = "Mean of Empirical and Recovered Parameters", y = "Parameter Difference\n(Empirical - Recovered)",
    color = "Task Type", shape = "Task Type"
    ) +
  guides(color = guide_legend(nrow = 2, title.vjust = 0.9)) +
  scale_color_manual(values = legend_cols, limits = legend_lims) +
  scale_shape_manual(values = legend_shapes, limits = legend_lims) +
  facet_wrap(~par, labeller = label_parsed, nrow = 1) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", size = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

par_ba_plot

# Behavioral Effects ------------------------------------------------------

# Define factor levels
temp_ord <- c("Prospective", "Retrospective") # temporal orientation
task_ord <- c("Hypothetical", "Real") # task type

sim$id <- factor(sim$id)
sim$temp_ori <- factor(sim$temp_ori, levels = temp_ord)
sim$task_type <- factor(sim$task_type, levels = task_ord)

# Fit a glmm model to effort acceptance, temporal orientation, task type, and data type
ctrl <- glmmTMBControl(parallel = (parallel::detectCores() - 1))
sim_mod <-
  glmmTMB(
    EffortfulOptionChosen ~ (X1 + E2) * temp_ori * task_type + session + (1|id),
    data = sim, family = binomial, control = ctrl
  )

sim_mod_anova <- car::Anova(sim_mod)
print(sim_mod_anova)
write.csv(as.data.frame(sim_mod_anova), "output/tables/simulated_model_anova.csv")

sim_prob <- modelbased::estimate_means(sim_mod, at = c("temp_ori", "task_type", "E2 = [0, 1]"), length = 100)

sim_dprob <- # calculate empirical effort acceptance probabilities
  sim %>%
  group_by(id, E2, temp_ori, task_type) %>%
  summarize(mean_prob = mean(EffortfulOptionChosen))

# Plot fitted effort acceptance probabilities by task type
sim_prob_plot <-
  sim_prob %>%
  ggplot(aes(x = E2, y = Probability, color = temp_ori, fill = temp_ori, shape = temp_ori)) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ggbeeswarm::geom_beeswarm(data = sim_dprob, aes(y = mean_prob), size = 1, cex = 1.5, alpha = 0.45) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), linetype = "blank", alpha = 0.15) +
  geom_line(linewidth = 1) +
  labs(
    title = "Effortful Option Acceptance by Effort Intensity in Simulated Agents\n",
    x = "Proportion of Maximum Effort", y = "P(Choose Effortful Option)",
    color = "Temporal Orientation", fill = "Temporal Orientation",
    linetype = "Task Type", shape = "Task Type"
  ) +
  guides(linetype = "none", shape = "none") +
  scale_color_manual(values = temp_ori_color) +
  scale_fill_manual(values = temp_ori_color) +
  scale_shape_manual(values = c(1, 2)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  cowplot::theme_cowplot() +
  facet_grid(~task_type) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", size = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

sim_prob_plot

# Calculate and visualize linear trends
sim_lt <- emmeans::emtrends(sim_mod, pairwise~task_type|temp_ori, "E2", regrid = "response", infer = TRUE, adjust = "fdr")
sim_lt_plot <-
  as.data.frame(sim_lt$emtrends) %>%
  ggplot(aes(x = fct_rev(task_type), y = E2.trend, color = temp_ori)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.15,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Effect of Effort\n",
    x = "Task Type", y = expression(beta[E]),
    color = "Task Type"
  ) +
  guides(color = "none") +
  coord_flip(ylim = c(-0.5, 0.5)) +
  scale_color_manual(values = temp_ori_color) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5)
  )

sim_lt_plot

# Results Visualization ---------------------------------------------------

# Combine plots for publication figure
fig_sim <-
  (sim_prob_plot / par_ba_plot) +
  plot_annotation(tag_levels = "a") &
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

fig_sim

cowplot::ggsave2("output/plots/Fig6.pdf", fig_sim, width = 10, height = 7, scale = 1.25)
