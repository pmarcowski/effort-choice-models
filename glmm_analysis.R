#' Effort Choice Models Analysis Pipeline
#'
#' @description
#' This script analyzes choice behavior in effort-based decision making tasks using
#' generalized linear mixed models (GLMM). The analysis pipeline includes:
#' - Data preparation and factor level specification
#' - Choice pattern analysis and visualization
#' - GLMM model fitting for effort acceptance
#' - Marginal means analysis and visualization
#' - Linear trends analysis
#' - Power and sensitivity analysis
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-03-07
#' @copyright (c) 2023 Przemyslaw Marcowski

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

# Data Preparation --------------------------------------------------------

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

# Choice Pattern Analysis -------------------------------------------------

# Calculate and visualize choice proportions and preference transitions
choice_patterns <- dt %>%
  group_by(id, temp_ori) %>%
  reframe(
    effort_level = unique(E2),
    choice_prop = sapply(effort_level, function(e) mean(EffortfulOptionChosen[E2 == e]))
  ) %>%
  group_by(id, temp_ori) %>%
  reframe(
    lower_effort = effort_level,
    higher_effort = list(effort_level),
    lower_choice = choice_prop,
    higher_choice = list(choice_prop)
  ) %>%
  unnest(cols = c(higher_effort, higher_choice)) %>%
  filter(higher_effort > lower_effort) %>%
  mutate(
    pattern = case_when(
      lower_choice <= 0.5 & higher_choice > 0.5 ~ "Positive",
      lower_choice > 0.5 & higher_choice <= 0.5 ~ "Negative",
      TRUE ~ "No Change"
    )
  ) %>%
  group_by(id, temp_ori) %>%
  mutate(
    pattern_type = case_when(
      any(pattern == "Positive") ~ "Positive",
      any(pattern == "Negative") ~ "Negative",
      TRUE ~ "No Change"
    )
  ) %>%
  ungroup()

# Create data for violin plot
choice_violin_data <- choice_patterns %>%
  pivot_longer(
    cols = c("lower_choice", "higher_choice"),
    names_to = "effort_level",
    values_to = "prop_chosen"
  ) %>%
  mutate(x_level = factor(if_else(effort_level == "lower_choice", "Low", "High"),
                          levels = c("Low", "High")))

# Visualize preference shifts
choice_patterns_plot <- choice_patterns %>%
  ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_segment(
    aes(x = "Low", xend = "High",
        y = lower_choice, yend = higher_choice,
        color = pattern_type,
        group = id),
    alpha = 0.4
    ) +
  geom_point(aes(x = "Low", y = lower_choice, color = pattern_type), size = 2) +
  geom_point(aes(x = "High", y = higher_choice, color = pattern_type), size = 2) +
  geom_violin(
    data = choice_violin_data,
    aes(x = x_level, y = prop_chosen),
    alpha = 0.2
    ) +
  facet_wrap(~temp_ori) +
  scale_color_manual(values = plot_colors[c(2, 5, 3)]) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
    ) +
  labs(
    x = "Proportion of Maximum Effort",
    y = "P(Choose Effortful Option)",
    color = "Preference Pattern"
    ) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(color = "transparent", fill = "transparent"))

choice_patterns_plot

# Calculate patterns summary
choice_patterns_summary <- choice_patterns %>%
  group_by(temp_ori, pattern_type) %>%
  summarise(
    n = n(),
    prop = n / sum(n),
    mean_prop = mean(prop),
    .groups = "drop"
  )

knitr::kable(choice_patterns_summary)

# GLMM Analysis -----------------------------------------------------------

# Fit mixed model for effort acceptance with temporal orientation and task type
ctrl <- glmmTMBControl(parallel = (parallel::detectCores() - 1))
choice_mod <-
  glmmTMB(
    EffortfulOptionChosen ~ (X1 + E2) * temp_ori * task_type + session + (1 | id),
    data = dt, family = binomial, control = ctrl
  )

choice_anova <- car::Anova(choice_mod)
print(choice_anova)
write.csv(as.data.frame(choice_anova), "output/tables/choice_model_anova.csv")

## Marginal Means ----

# Calculate and visualize estimated marginal means
em1 <- emmeans(choice_mod, pairwise ~ temp_ori, regrid = "response", infer = TRUE, adjust = "fdr")
em2 <- emmeans(choice_mod, pairwise ~ temp_ori | task_type, regrid = "response", infer = TRUE, adjust = "fdr")
choice_em <- emmeans(choice_mod, pairwise ~ task_type | temp_ori, regrid = "response", infer = TRUE, adjust = "fdr")

# Extract means and contrasts
choice_means <- as.data.frame(choice_em$emmeans)
choice_contrasts <- as.data.frame(choice_em$contrasts)

print(choice_means)
print(choice_contrasts)
print(em1)
print(em2)

write.csv(choice_means, "output/tables/choice_model_means.csv")
write.csv(choice_contrasts, "output/tables/choice_model_contrasts.csv")

choice_em <- choice_means  # Use means for plotting

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

## Linear Trends ----

# Analyze effort slopes across conditions
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

## Sensitivity Analysis ----

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

# Results Visualization ---------------------------------------------------

# Combine plots for publication figure
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
