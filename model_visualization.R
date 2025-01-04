#' Model Visualization for Effort Choice Theory
#'
#' @description
#' This script generates theoretical visualizations of effort-based value functions.
#' The visualization demonstrates:
#' - Four profile types (Decreasing, Increasing, Decreasing-Increasing, Increasing-Decreasing)
#' - Positive and negative system contributions
#' - Combined system effects on subjective value
#'
#' @author Przemyslaw Marcowski, PhD p.marcowski@gmail.com
#' @date 2023-06-05
#' @copyright (c) 2023 Przemyslaw Marcowski

# Load packages
library(tidyverse)
library(patchwork)

# Define colors for plotting
colors <- c(
  "#708C98FF", "#A73030FF", "#15983DFF",
  "#0C5BB0FF", "#FEC10BFF", "#3B3B3BFF"
)

# Generate theoretical value function plots
model_plot <-
  bind_rows(
    tibble(profile = "Decreasing", par1 = 2, par2 = 2, par3 = 2, par4 = 2, par5 = 0.2),
    tibble(profile = "Increasing", par1 = 2, par2 = 2, par3 = 2, par4 = 2, par5 = 0.8),
    tibble(profile = "Decreasing-Increasing", par1 = 3, par2 = 3, par3 = 10, par4 = 3, par5 = 0.5),
    tibble(profile = "Increasing-Decreasing", par1 = 4, par2 = 3, par3 = 1.5, par4 = 3, par5 = 0.5)
  ) %>%
  cross_join(tibble(E = seq(0, 1, 0.01))) %>%
  mutate(
    profile = factor(profile,
                     levels = c("Decreasing", "Increasing",
                                "Decreasing-Increasing", "Increasing-Decreasing")),
    positive = 1 * (1 + par5 * (par1 * E^par3)),
    negative = 1 * (1 - (1 - par5) * (par2 * E^par4)),
    combined = 1 * (1 + (par5 * (par1 * E^par3) - (1 - par5) * (par2 * E^par4)))
  ) %>%
  select(profile, E, positive, negative, combined) %>%
  pivot_longer(
    cols = c(positive, negative, combined),
    names_to = "system",
    values_to = "value"
  ) %>%
  mutate(
    system = factor(system,
                    levels = c("positive", "negative", "combined"),
                    labels = c("Positive", "Negative", "Combined"))
  ) %>%
  ggplot(aes(x = E, y = value, color = system, linewidth = system)) +
  geom_line(aes(linetype = system)) +
  scale_linewidth_manual(
    values = c(0.5, 0.5, 1)
    ) +
  scale_color_manual(
    values = c(colors[3], colors[2], "black")
  ) +
  scale_linetype_manual(
    values = c("dashed", "dashed", "solid")
  ) +
  labs(
    x = "Proportion of Maximum Effort",
    y = "Ratio of Subjective Value",
    color = "System Contribution",
    linetype = "System Contribution",
    linewidth = "System Contribution"
  ) +
  coord_cartesian(ylim = c(0, 3), expand = FALSE) +
  facet_wrap(~profile, nrow = 1) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    panel.spacing = unit(2, "lines"),
    plot.margin = unit(c(1, 1.5, 1, 1), "lines"),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

model_plot
cowplot::ggsave2("output/plots/Fig1.pdf", model_plot, width = 10, height = 4)
