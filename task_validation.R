# Title: Behavioral task demand analysis for effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-03-22
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(glmmTMB)

# Read and prepare data
task_test <- 
  read_csv("data/task_test.csv") %>%
  select(subjID, sex, age, trial, effort_level, component, rating) %>%
  filter(component %in% c("Effort", "Temporal")) %>%
  mutate(
    subjID = as.factor(subjID),
    effort_level = factor(effort_level, levels = c("Low", "Medium", "High")),
    component = factor(component, levels = c("Effort", "Temporal"))
  ) %>%
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

demand_mod <- glmmTMB(Effort ~ effort_level + Temporal + (1|subjID), data = task_test)
summary(demand_mod)
car::Anova(demand_mod)

# Visualize effort and temporal demand ratings for each level of effort
effort_demand_plot <- task_test %>%
  gather(
    component, rating, 
    -subjID, -sex, -age, -trial, -effort_level
    ) %>%
  group_by(effort_level, component) %>%
  summarize(
    mean_rating = mean(rating),
    se = sd(rating) / sqrt(n()),
    lower.CL = mean_rating - 1.96 * se,
    upper.CL = mean_rating + 1.96 * se
  ) %>%
  ungroup() %>%
  ggplot(aes(x = effort_level, y = mean_rating, group = component, color = component)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
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
