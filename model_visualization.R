# Title: Generating theory plots for the effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-06-05
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(patchwork)

# Define colors for plotting
colors <- c(
  "#708C98FF", "#A73030FF", "#15983DFF",
  "#0C5BB0FF", "#FEC10BFF", "#3B3B3BFF"
)

# Experiment 1 ------------------------------------------------------------

mod_plot <- 
  bind_rows(
    tibble(eval = 1, par1 = 0, par2 = 2, par3 = 0, par4 = 2, par5 = 0, lab = "Decreasing"),
    tibble(eval = 2, par1 = -2, par2 = 0, par3 = .5, par4 = 0, par5 = 1, lab = "Increasing"),
    tibble(eval = 3, par1 = -3, par2 = 3, par3 = 15, par4 = 3, par5 = .5, lab = "Decreasing-Increasing"),
    tibble(eval = 4, par1 = 3.5, par2 = -3.5, par3 = 2, par4 = .7, par5 = .5, lab = "Increasing-Decreasing")
  ) %>%
  left_join(tibble(E = seq(0, 1, .01)), by = character()) %>%
  mutate(
    across(eval, factor, levels = c(1, 2, 3, 4)),
    sv = 1 * (1 - (par5 * (par1 * E^par3) + (1 - par5) * (par2 * E^par4)))
  ) %>%
  ggplot(aes(x = E, y = sv, group = eval)) +
  geom_line(color = "gold") +
  geom_text(
    data = . %>% group_by(eval) %>% slice(1), 
    aes(label = lab), x = c(.4, .2, .775, .45), y = c(.4, 2.225, .8, 1.825), 
    color = "black", size = 4, parse = TRUE
  ) +
  labs(
    x = "Proportion of Maximum Effort", 
    y = "Proportion of Subjective Value") +
  scale_linetype_discrete(labels = c(
    "1" = "Decreasing", 
    "2" = "Increasing",
    "3" = "Decreasing-Increasing", 
    "4" = "Increasing-Decreasing"
  )) +
  coord_cartesian(ylim = c(0, 3), expand = FALSE) +
  cowplot::theme_cowplot() +
  theme(
    aspect.ratio = 1, 
    legend.position = "none", 
    legend.text.align = 0
  )

mod_plot
cowplot::ggsave2("output/plots/", "model.pdf", mod_plot, width = 3, height = 3, scale = 1.2)
