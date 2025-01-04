#' Task Validation Analysis for Effort Choice Study
#'
#' @description
#' This script analyzes behavioral task demands and choice evaluations.
#' The analysis pipeline includes:
#' - Analysis of effort and temporal demands across effort levels
#' - Relationship between different types of task demands
#' - Analysis of post-choice evaluations
#' - Visualization of demand characteristics and choice evaluations
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-03-22
#' @copyright (c) 2023 Przemyslaw Marcowski

# Load packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

# Local source
source("R/colors.R")

# Define factor levels
temp_ord <- c("Prospective", "Retrospective") # temporal orientation
task_ord <- c("Hypothetical", "Real") # task type
session_ord <- c("1", "2") # session num
chosen_ord <- c("Payoff", "Item") # option chosen
eval_ord <- c("choice_certain", "item_liking", "payoff_liking") # evaluation type
eval_lab <- c("Choice Certainty", "Item Liking", "Payoff Liking")

# Plotting parameters
temp_ori_color <- plot_colors[c(4, 3)] # colors
col_width <- 0.375 # column widths

# Demand Analysis ---------------------------------------------------------

# Read and prepare task validation data
task_test <-
  read_csv("data/task_test.csv") %>%
  select(subjID, sex, age, trial, effort_level, component, rating) %>%
  filter(component %in% c("Effort", "Temporal")) %>%
  # Convert categorical variables to factors with specified levels
  mutate(
    subjID = as.factor(subjID),
    effort_level = factor(effort_level, levels = c("Low", "Medium", "High")),
    component = factor(component, levels = c("Effort", "Temporal"))
  ) %>%
  # Create wide format data with effort and temporal ratings as columns
  group_by(subjID, trial, effort_level, component) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  spread(component, rating) %>%
  select(-idx)

# Inspect participant demographics
task_test %>%
  dplyr::select(subjID, sex, age) %>%
  summarize(
    n_male = n_distinct(subjID[sex == "Male"]),
    n_female = n_distinct(subjID[sex == "Female"]),
    mean_age = mean(age),
    sd_age = sd(age)
    ) %>%
  knitr::kable(digits = 2)

# Fit mixed model to analyze effort-temporal relationship
demand_mod <- glmmTMB(Effort ~ effort_level + Temporal + (1|subjID), data = task_test)
summary(demand_mod)
demand_anova <- car::Anova(demand_mod)
print(demand_anova)
write.csv(as.data.frame(demand_anova), "output/tables/demand_model_anova.csv")

# Task Relationship Analysis ----------------------------------------------

# Visualize effort and temporal demand ratings for each level of effort
effort_demand_means <- task_test %>%
  gather(
    component, rating,
    -subjID, -sex, -age, -trial, -effort_level
    ) %>%
  group_by(effort_level, component) %>%
  summarize(
    mean_rating = mean(rating),
    se = sd(rating) / sqrt(n()),          # Standard error
    lower_CL = mean_rating - 1.96 * se,   # 95% CI lower bound
    upper_CL = mean_rating + 1.96 * se    # 95% CI upper bound
  ) %>%
  ungroup()

  knitr::kable(effort_demand_means, digits = 3)
  write.csv(as.data.frame(effort_demand_means), "output/tables/demand_means.csv")

  # Create demand rating plot
effort_demand_plot <- effort_demand_means %>%
  ggplot(aes(x = effort_level, y = mean_rating, group = component, color = component)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower_CL, ymax = upper_CL), width = 0.1) +
  labs(
    title = "Effort and Temporal Demand\nRating by Effort Level",
    x = "Effort Level", y = "Mean Rating",
    color = "Demand"
  ) +
  coord_cartesian(ylim = c(1, 5)) +
  see::scale_color_see() +
  cowplot::theme_cowplot() +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5)
    )

effort_demand_plot
ggsave("output/plots/FigS2.pdf", effort_demand_plot, width = 4, height = 4)

# Analyze relationship between temporal and effort demands
relationship_plot <- ggplot(task_test, aes(x = Temporal, y = Effort, color = effort_level)) +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ effort_level) +
  labs(
    title = "Relationship between Temporal\nand Effort Demand by Effort Level",
    x = "Temporal Demand", y = "Effort Demand"
  ) +
  see::scale_color_see() +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

relationship_plot

# Choice Evaluation -------------------------------------------------------

# Read and prepare choice evaluation data
last_choice <- read.table("data/processed/last_choice.txt") %>%
  select(id, session, task_type, temp_ori, E2, indiff, chosen, choice_certain, item_liking, payoff_liking) %>%
  gather(eval_type, response, -id, -session, -task_type, -temp_ori, -E2, -indiff, -chosen) %>%
  mutate(
    session = factor(session, levels = session_ord),
    temp_ori = factor(temp_ori, levels = temp_ord),
    task_type = factor(task_type, levels = task_ord),
    chosen = factor(chosen, levels = chosen_ord),
    eval_type = factor(
      eval_type,
      levels = eval_ord,
      labels = eval_lab
      )
    )

# Fit mixed model for choice evaluations
eval_mod <- glmmTMB(
  response ~ temp_ori * task_type * chosen * eval_type + # Fixed effects
    E2 + indiff + session +                              # Control variables
    (1|id) + (1|eval_type),                              # Random effects
  data = last_choice
)

eval_anova <- car::Anova(eval_mod)
print(eval_anova)
write.csv(as.data.frame(eval_anova), "output/tables/eval_model_anova.csv")

# Extract means and contrasts from emmeans calls
eval_type_means <- emmeans(
  eval_mod, pairwise ~ eval_type | chosen,
  regrid = "response", infer = TRUE, adjust = "fdr",
  at = list(eval_type = c("Item Liking", "Payoff Liking"))
)

eval_chosen_means <- emmeans(
  eval_mod, pairwise ~ chosen | eval_type,
  regrid = "response", infer = TRUE, adjust = "fdr",
  at = list(eval_type = "Choice Certainty")
)

# Save extracted results
write.csv(as.data.frame(eval_type_means$emmeans), "output/tables/eval_type_means.csv")
write.csv(as.data.frame(eval_type_means$contrasts), "output/tables/eval_type_contrasts.csv")
write.csv(as.data.frame(eval_chosen_means$emmeans), "output/tables/chosen_means.csv")
write.csv(as.data.frame(eval_chosen_means$contrasts), "output/tables/chosen_contrasts.csv")

# Calculate estimated marginal means for key comparisons
eval_em <- emmeans(
  eval_mod,
  pairwise ~ chosen | temp_ori * task_type * eval_type,
  regrid = "response",
  infer = TRUE,
  adjust = "fdr"
)

# Extract means and contrasts
eval_means <- as.data.frame(eval_em$emmeans)
eval_contrasts <- as.data.frame(eval_em$contrasts)

# Save extracted results
write.csv(as.data.frame(eval_means$emmeans), "output/tables/eval_means.csv")
write.csv(as.data.frame(eval_contrasts$contrasts), "output/tables/eval_contrasts.csv")

# Results Visualization ---------------------------------------------------

# Create conditions in order
eval_means$cond <- factor(with(eval_means, paste0(chosen, " (", temp_ori, ")")),
                           levels = c("Payoff (Prospective)",
                                      "Item (Prospective)",
                                      "Payoff (Retrospective)",
                                      "Item (Retrospective)"))

legend_lims <- levels(eval_means$cond)
legend_cols <- c(rep(temp_ori_color[1], 2), rep(temp_ori_color[2], 2))
legend_alpha <- c(0.5, 1, 0.5, 1)

# Create evaluation plot with grouped bars and error bars
eval_plot <- eval_means %>%
  ggplot(aes(
    x = eval_type, y = emmean,
    group = interaction(temp_ori, chosen),
    fill = cond)
  ) +
  geom_col(
    aes(alpha = cond),
    width = col_width, color = "black",
    position = position_dodge(width = col_width)
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = col_width),
    width = 0.1
  ) +
  scale_fill_manual(values = legend_cols, limits = legend_lims) +
  scale_alpha_manual(values = legend_alpha, limits = legend_lims, guide = "none") +
  labs(
    title = "Choice Evaluation by Choice Context",
    x = "Evaluation Type", y = "Mean Rating"
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      ncol = 2,
      title = "Option Chosen",
      override.aes = list(alpha = legend_alpha)
    )
  ) +
  coord_cartesian(ylim = c(0, 7)) +
  cowplot::theme_cowplot() +
  facet_grid(temp_ori ~ task_type) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

eval_plot
ggsave("output/plots/FigS4.pdf", eval_plot, width = 10, height = 7)
