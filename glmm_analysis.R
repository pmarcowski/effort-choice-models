# Title: Choice analysis for effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-03-07
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(purrr)
library(simr)
library(boot)
library(broom)
library(patchwork)

# Local source
source("R/helpers.R")
source("R/colors.R")

# Plotting options
temp_ori_color <- plot_colors[c(4, 3)] # colors
col_width <- 0.375 # column widths

# Data --------------------------------------------------------------------

# Define factor levels
temp_ord <- c("Prospective", "Retrospective") # temporal orientation
task_ord <- c("Hypothetical", "Real") # task type
session_ord <- c("1", "2") # session num

# Retrieve final data
dt <- read.table("data/processed/choice_prepared.txt")
dt$id <- factor(dt$id)
dt$session <- factor(dt$session, levels = session_ord)
dt$temp_ori <- factor(dt$temp_ori, levels = temp_ord)
dt$task_type <- factor(dt$task_type, levels = task_ord)

# GLMM: Effort acceptance -------------------------------------------------

# Fit a glmm model to effort acceptance, temporal orientation, and task type
ctrl <- glmmTMBControl(parallel = (parallel::detectCores() - 1))
choice_mod <-
  glmmTMB(
    EffortfulOptionChosen ~ (X1 + E2) * temp_ori * task_type + session + (1 | id),
    data = dt, family = binomial, control = ctrl
  )

car::Anova(choice_mod)

### Marginal means ----

emmeans(choice_mod, pairwise ~ temp_ori, regrid = "response", infer = TRUE, adjust = "fdr")
emmeans(choice_mod, pairwise ~ temp_ori | task_type, regrid = "response", infer = TRUE, adjust = "fdr")

choice_em <- emmeans(choice_mod, pairwise ~ task_type | temp_ori, regrid = "response", infer = TRUE, adjust = "fdr")
choice_em <- as.data.frame(choice_em$emmeans)
choice_em$cond <- with(choice_em, paste0(task_type, " (", temp_ori, ")"))
legend_lims <- sort(unique(choice_em$cond))
legend_cols <- rep(temp_ori_color, 2)
legend_alpha <- rep(c(0.4, 1), each = 2)

choice_em_plot <- # plot average probabilities
  choice_em %>%
  ggplot(aes(x = temp_ori, y = prob, group = task_type, fill = cond)) +
  geom_col(aes(alpha = cond), width = col_width, color = "black", position = position_dodge(width = col_width)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1, position = position_dodge(width = col_width)) +
  labs(
    title = "Effortful Option Acceptance\nby Choice Context",
    x = "Temporal Orientation", y = "P(Choose Effortful Option)",
    fill = "Task Type", alpha = "Task Type"
  ) +
  scale_fill_manual(values = legend_cols, limits = legend_lims) +
  scale_alpha_manual(values = legend_alpha, limits = legend_lims) +
  guides(fill = guide_legend(nrow = 2, title.vjust = 0.9)) +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  ) +
  # Draw temporal orientation comparison
  annotate("segment", x = 0.9975, xend = 2.0025, y = 0.925, yend = 0.925, colour = "black") +
  annotate("segment", x = 1, xend = 1, y = 0.7, yend = 0.925, colour = "black") +
  annotate("segment", x = 2, xend = 2, y = 0.86, yend = 0.925, colour = "black") +
  annotate("text", x = 1.5, y = 0.95, label = "**") +
  # Draw task comparison for prospective condition
  annotate("segment", x = 0.9, xend = 1.0875, y = 0.65, yend = 0.65, colour = "black") +
  annotate("segment", x = 0.9, xend = 0.9, y = 0.62, yend = 0.65, colour = "black") +
  annotate("segment", x = 1.0875, xend = 1.0875, y = 0.62, yend = 0.65, colour = "black") +
  annotate("text", x = 1, y = 0.68, label = "ns", size = 3) +
  # Draw task comparison for retrospective condition
  annotate("segment", x = 1.9, xend = 2.0875, y = 0.81, yend = 0.81, colour = "black") +
  annotate("segment", x = 1.9, xend = 1.9, y = 0.785, yend = 0.81, colour = "black") +
  annotate("segment", x = 2.0875, xend = 2.0875, y = 0.785, yend = 0.81, colour = "black") +
  annotate("text", x = 2, y = 0.84, label = "ns", size = 3)

choice_em_plot

choice_prob <- modelbased::estimate_means(choice_mod, at = c("temp_ori", "task_type", "E2 = [0, 1]"), length = 100)

choice_dprob <- # calculate empirical effort acceptance probabilities
  dt %>%
  group_by(id, E2, temp_ori, task_type) %>%
  summarize(mean_prob = mean(EffortfulOptionChosen))

# Plot fitted effort acceptance probabilities by task type
choice_prob_plot <-
  choice_prob %>%
  ggplot(aes(x = E2, y = Probability, color = temp_ori, fill = temp_ori, shape = temp_ori)) +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ggbeeswarm::geom_beeswarm(data = choice_dprob, aes(y = mean_prob), size = 1, cex = 1.5, alpha = 0.45) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), linetype = "blank", alpha = 0.15) +
  geom_line(linewidth = 1) +
  labs(
    title = "Effortful Option Acceptance by Effort Intensity\n",
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

choice_prob_plot

### Linear trends ----

emtrends(choice_mod, ~1, "X1", regrid = "response", infer = TRUE, adjust = "fdr")
emtrends(choice_mod, ~1, "E2", regrid = "response", infer = TRUE, adjust = "fdr")

choice_lt1 <- emtrends(choice_mod, pairwise ~ task_type | temp_ori, "E2", regrid = "response", infer = TRUE, adjust = "fdr")
choice_lt2 <- emtrends(choice_mod, pairwise ~ temp_ori | task_type, "E2", regrid = "response", infer = TRUE, adjust = "fdr")

choice_lt_plot <-
  as.data.frame(choice_lt1$emtrends) %>%
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
    x = "Task Type", y = expression(hat(theta)[Effort]),
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

choice_lt_plot

### Sensitivity analysis ----

# Define effect sizes and effect names
log_odds_ratios <- c(0.1, 0.2, 0.5, 0.8)
effect_names <- c(
  "X1",
  "E2",
  "temp_ori",
  "task_type",
  "X1:temp_ori",
  "E2:temp_ori",
  "temp_ori:task_type"
)

# Calculate power for all combinations of effects and effect sizes
power_results <- expand_grid(effect = effect_names, log_odds_ratio = log_odds_ratios) %>%
  mutate(result = map2(effect, log_odds_ratio, calculate_power)) %>%
  unnest_wider(result)

# Inspect results
knitr::kable(power_results, digits = 4)

# Plot results
ggplot(power_results, aes(x = odds_ratio, y = power, color = effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_log10(breaks = power_results$odds_ratio, labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "Sensitivity Analysis",
    x = "Odds Ratio", y = "Power", 
    color = "Effect"
  ) +
  see::scale_color_see() +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "transparent")
  )

# Results -----------------------------------------------------------------

# Effort acceptance

p2a <- choice_em_plot
p2b <- choice_prob_plot
p2c <- choice_lt_plot

fig_choice <-
  (p2a + p2b + p2c) +
    plot_layout(widths = c(0.5, 1, 0.5), guides = "collect") +
    plot_annotation(tag_levels = "a") &
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )

fig_choice
cowplot::ggsave2("output/plots/Fig3.pdf", fig_choice, width = 10, height = 3.5, scale = 1.2)
