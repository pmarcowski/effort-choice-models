# Title: Model inference for effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-03-07
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(brms)
library(furrr)
library(boot)
library(broom)
library(patchwork)

# Local source
source("R/models.R")
source("R/helpers.R")
source("R/colors.R")

# Define factor levels
temp_ord <- c("Prospective", "Retrospective") # temporal orientation
task_ord <- c("Hypothetical", "Real") # task type

# Plotting options
temp_ori_color <- plot_colors[c(4, 3)] # colors
col_width <- 0.375 # column widths

# Define parameter labels to display on plots
par_labels <- c(expression(delta[1], gamma[1], delta[2], gamma[2], omega, tau, delta[1]*gamma[1], delta[2]*gamma[2]))

# Number of ootstrap samples
nboot <- 1000

# Number of cores for parallelization
ncores <- parallel::detectCores() - 1

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

# Data --------------------------------------------------------------------

# Retrieve output
cv <- read.table("output/models/processed/cv.txt") # model cross-validation
fits <- read.table("output/models/processed/fits.txt") # model fits
pars <- fits[, c("id", "temp_ori", "task_type", "model", sprintf("par%i", 1:6))] # fitted parameters
pars_sel <- pars[pars$model == "DPOWER", ] # DPOWER parameters
pars_sel$term1 <- with(pars_sel, abs(par1 * 1^par3))
pars_sel$term2 <- with(pars_sel, abs(par2 * 1^par4))

# Get results across repetitions for each individual per condition
res <-
  cv %>%
  mutate(pcor = 1 - z_o) %>%
  group_by(id, temp_ori, repn) %>%
  mutate(rank_test = dense_rank(logloss)) %>%
  ungroup()

# Behavioral modeling -----------------------------------------------------

## Model loss ----

# Summarize loss across repetitions
# Use logloss, p-correct
loss <-
  res %>%
  group_by(temp_ori, model) %>%
  summarise(
    n = n(),
    logloss = mean(logloss),
    pcor = mean(pcor),
    mean_rank = mean(rank_test)
  ) %>%
  ungroup() %>%
  arrange(temp_ori, logloss)

knitr::kable(loss, digits = 3)
mod_order <- levels(forcats::fct_reorder(loss$model, -loss$mean_rank))
res$model <- factor(res$model, levels = mod_order)
loss$model <- factor(loss$model, levels = mod_order)

# Plot model loss
loss_plot <-
  res %>%
  ggplot(aes(x = model, y = logloss, color = temp_ori, fill = temp_ori)) +
  geom_hline(yintercept = -log(0.5), linetype = "dashed") +
  geom_point(size = 0.5, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.75)) +
  geom_boxplot(
    outlier.shape = NA, alpha = 0.8, width = 0.5, color = "black",
    position = position_dodge(width = 0.75)
  ) +
  stat_boxplot(
    geom = "errorbar", color = "black", width = 0.2,
    position = position_dodge(width = 0.75)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 2, color = "black", position = position_dodge(width = 0.75)) +
  geom_text(
    data = summarise(
      group_by(res, temp_ori, model), 
      y = mean(logloss), 
      pos = 0.1 + (quantile(logloss, 0.75) + 1.5 * IQR(logloss))
      ),
    aes(y = pos, label = gsub("0\\.", "\\.", format(round(y, 3)))),
    position = position_dodge(width = 0.75),
    color = "black"
  ) +
  labs(title = "Prediction Error", x = "Model", y = "Log-Loss") +
  guides(fill = guide_legend(title = "Temporal Orientation"), color = "none") +
  scale_color_manual(values = temp_ori_color) +
  scale_fill_manual(values = temp_ori_color) +
  scale_x_discrete(limits = mod_order) +
  scale_y_continuous(breaks = seq(0, 1.6, 0.4)) +
  coord_cartesian(ylim = c(0, 1.6)) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

loss_plot

### Bootstrap test ----

# Get bootstrapped differences of mean loss
boot_loss_file <- "output/models/boot/loss.rds"
if (!file.exists(boot_loss_file)) { # check if checkpoint exists
  cl <- makeClusterPSOCK(ncores)
  plan(cluster, workers = cl)
  
  loss_boot <-
    res %>%
    select(temp_ori, id, repn, model, logloss) %>%
    left_join(
      res %>% select(temp_ori, id, repn, model, logloss),
      by = c("temp_ori", "id", "repn"), suffix = c("1", "2"),
      multiple = "all"
    ) %>%
    filter(model1 != model2) %>%
    group_nest(temp_ori, model1, model2) %>%
    mutate(
      n = map(data, nrow),
      boots = future_map(
        data, ~boot(
          data = ., function(x, i) {
            d <- as.data.frame(x)[i, ]
            ms <- sapply(list(d$logloss1, d$logloss2), mean)
            return(c(diff(ms)))
          }, R = nboot
        ),
        .progress = TRUE,
        .options = furrr_options(seed = TRUE)
      )
    ) %>%
    select(-data)

  parallel::stopCluster(cl)

  # Get confidence intervals for differences of means
  cl <- makeClusterPSOCK(ncores)
  plan(cluster, workers = cl)
  
  loss_ci <-
    loss_boot %>%
    mutate(
      ci = future_map(
        boots, tidy,
        conf.int = TRUE,
        conf.level = 0.95,
        conf.method = "perc",
        .progress = TRUE,
        .options = furrr_options(seed = TRUE, packages = "broom")
      )
    ) %>%
    select(-boots) %>%
    unnest(cols = ci)
  
  parallel::stopCluster(cl)
  
  save(loss_boot, loss_ci, file = boot_loss_file) # save checkpoint
} else { # reload checkpoint
  load(file = boot_loss_file)
}

# Format loss bootstrap results
loss_ci_temp <-
  loss_ci %>%
  mutate(
    t = (statistic - 0) / std.error,
    p_val = 2 * pt(-abs(t), as.integer(n) - 1),
    is_diff = if_else(conf.low <= 0 & conf.high >= 0, FALSE, TRUE),
    across(c(model1, model2), forcats::fct_relevel, mod_order)
  ) %>%
  group_by(temp_ori) %>%
  nest() %>%
  mutate(
    casted = map(data, reshape2::dcast, model1 ~ model2, value.var = "p_val"),
    casted = map(casted, column_to_rownames, "model1"),
    casted = map(casted, ~ replace(.x, upper.tri(.), NA)),
    casted = map(casted, rownames_to_column, var = "model1"),
    melted = map(casted, reshape2::melt, id.var = "model1"),
    melted = map(melted, rename, c("model2" = "variable", "p_val" = "value")),
    melted = map(melted, mutate, across(c(model1, model2), forcats::fct_relevel, mod_order))
  ) %>%
  select(temp_ori, melted) %>%
  unnest(melted) %>%
  ungroup()

loss_ci_temp$p_val_adj <- p.adjust(loss_ci_temp$p_val, "fdr", length(loss_ci_temp$p_val[which(!is.na(loss_ci_temp$p_val))]))
loss_ci_temp$lab <- cut(loss_ci_temp$p_val_adj, c(-Inf, 0.001, 0.01, 0.05, Inf), c(".001", ".01", ".05", "ns"))

# Plot performance comparisons per condition
cs <- scales::seq_gradient_pal(plot_colors[2], "black", "Lab")(seq(0, 1, length.out = 4))
loss_test_plot <-
  loss_ci_temp %>%
  ggplot(aes(x = model1, y = reorder(model2, -as.numeric(model2)), fill = lab)) +
  geom_rect(ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Inf) +
  geom_tile(color = "black") +
  geom_text(
    data = loss,
    aes(y = model, label = paste(format(mean_rank, digits = 2, nsmall = 2))),
    x = 8.75, hjust = 0, inherit.aes = FALSE
  ) +
  labs(title = "Performance Comparisons", x = "Model", y = "", fill = "Significance") +
  scale_fill_manual(values = cs, na.value = "black", breaks = c(".001", ".01", ".05", "ns")) +
  # scale_fill_manual(
  #   name = "Difference",
  #   values = c(plot_colors[2], plot_colors[5]),
  #   breaks = c(FALSE, TRUE),
  #   labels = c("False", "True"),
  #   guide = guide_legend(),
  #   na.value = "black"
  #   ) +
  facet_wrap(~temp_ori, ncol = 2) +
  cowplot::theme_cowplot() +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.spacing.x = unit(2, "lines"),
    plot.margin = unit(c(1, 2, 1, 1), "lines"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black"),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

loss_test_plot

## Model frequencies ----

n_ranks <- 5 # set number of ranks to display
loss_ranks_freq <- # calculate model ranks
  res %>%
  group_by(temp_ori, model) %>%
  count(rank_test) %>%
  mutate(
    prop = n / sum(n),
    CI_low = lapply(n, prop.test, n = sum(n)),
    CI_high = sapply(CI_low, function(x) x$conf.int[2]),
    CI_low = sapply(CI_low, function(x) x$conf.int[1])
  ) %>%
  ungroup() %>%
  filter(rank_test %in% 1:n_ranks)

loss_ranks_freq %>% # inspect rank 1 frequencies
  filter(rank_test == 1) %>%
  arrange(temp_ori, -prop) %>%
  knitr::kable(digits = 3) 

relf_plot <- # plot model frequencies
  loss_ranks_freq %>%
  ggplot(aes(x = model, fill = temp_ori, alpha = as.factor(-rank_test))) +
  geom_col(
    data = loss_ranks_freq[loss_ranks_freq$temp_ori == "Prospective", ],
    aes(x = as.numeric(model) - col_width / 2, y = prop),
    color = "black",
    width = col_width,
    position = "stack"
  ) +
  geom_col(
    data = loss_ranks_freq[loss_ranks_freq$temp_ori == "Retrospective", ],
    aes(x = as.numeric(model) + col_width / 2, y = prop),
    color = "black",
    width = col_width,
    position = "stack"
  ) +
  labs(
    title = "Model Frequencies",
    x = "Model", y = "Rank Frequency",
    fill = "Temporal Orientation",
    alpha = "Model Rank"
  ) +
  scale_x_discrete(limits = mod_order) +
  scale_alpha_manual(values = seq(.25, 1, length.out = n_ranks)) +
  scale_fill_manual(values = temp_ori_color) +
  coord_cartesian(ylim = c(0, 1)) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "none",
    legend.justification = c(0.5, 0.5),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

relf_plot

## Choices predicted ----

# Plot choices correctly predicted
pcor_plot <-
  res %>%
  ggplot(aes(x = reorder(model, pcor), y = pcor, color = temp_ori, fill = temp_ori, group = temp_ori)) +
  stat_summary(fun = "mean", geom = "col", color = "black", width = col_width * 2, position = position_dodge(width = col_width * 2)) +
  # stat_summary(fun.data = mean_cl_normal, geom = "errorbar", color = "black", width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_fill_manual(values = temp_ori_color) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Choices Predicted", x = "Model", y = expression(paste(pi["correct"]))) +
  guides(fill = guide_legend(title = "Temporal Orientation"), color = "none") +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

pcor_plot

## Predictions ----

### Value functions ----

# Extract individual parameter estimates and plot value functions

pars_list <- split(pars_sel, pars_sel$id, drop = TRUE)
dat <- data.frame(E1 = 0, E2 = seq(0, 1, length.out = 100), X1 = 1, X2 = 1)
val_list <- list()
for (i in seq_along(pars_list)) {
  this_pars <- pars_list[[i]]
  model <- paste0("m", this_pars$model[1])
  id <- this_pars$id[1]
  temp_ori <- this_pars$temp_ori[1]
  task_type <- this_pars$task_type[1]
  par <- unlist(this_pars[sprintf("par%i", 1:6)])
  args <- list(par, dat, choice = TRUE)
  sv <- do.call(model, args)[, 3]
  out <- data.frame(id, temp_ori, task_type, model, E2 = dat$E2, sv)
  val_list[[length(val_list) + 1]] <- out
}

val <- do.call(rbind, val_list)

profiles <- # get value function profiles
  val %>%
  mutate(sv = as.numeric(sv)) %>%
  group_by(id, temp_ori) %>%
  summarise(
    # Get preference profiles
    profile = detect_sequences(sv),
    # Get function change points
    changepoints = as.integer(count_changepoints(sv)),
    profile = str_to_title(profile)
  )

val_profiles <- left_join(val, profiles) # combine subjective values and profiles

### Preference profiles ----

# Example cases
val_examples0 <- c("WS34HCRC2", "SM20HIHI1", "WS8HIRI1", "WS3HIHI2")

# Create labels for example profiles
val_examples_labs <-
  val_profiles %>%
  filter(id %in% val_examples0, sv > 0) %>%
  arrange(desc(E2)) %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup()

# Plot example preference profiles
profiles_plot <-
  val_profiles %>%
  filter(id %in% val_examples_labs$id) %>%
  ggplot(aes(x = E2, y = sv, group = id)) +
  geom_line(color = "gold") +
  ggrepel::geom_text_repel(
    data = val_examples_labs,
    aes(label = profile),
    segment.color = NA, nudge_y = 0.1
  ) +
  labs(
    x = "Proportion of Maximum Effort", 
    y = "Proportion of Subjective Value"
    ) +
  coord_cartesian(ylim = c(0, 3), expand = FALSE) +
  labs(title = "Preference Profiles") +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    # panel.border = element_rect(color = "black"),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

profiles_plot

profiles_freq <- # calculate profile proportions
  profiles %>%
  select(temp_ori, profile) %>%
  group_by(temp_ori) %>%
  count(profile) %>%
  mutate(
    prop = n / sum(n),
    CI_low = lapply(n, prop.test, n = sum(n)),
    CI_high = sapply(CI_low, function(x) x$conf.int[2]),
    CI_low = sapply(CI_low, function(x) x$conf.int[1])
  )

knitr::kable(arrange(profiles_freq, profile), digits = 2) # inspect proportions
profiles_tab <- xtabs(n ~ profile + temp_ori, data = profiles_freq) # create cross-table
profiles_test <- fisher.test(profiles_tab[c(1, 3), ]) # compare profile counts
profiles_test

profiles_freq_plot <- # plot profile proportions
  profiles_freq %>%
  ggplot(aes(x = profile, y = prop, fill = temp_ori)) +
  geom_col(width = col_width * 1.25, position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.1, position = position_dodge(width = col_width * 1.25)) +
  scale_x_discrete(
    limits = c("Decreasing", "Decreasing-Increasing", "Increasing", "Increasing-Decreasing"),
    labels = c("D", "D-I", "I", "I-D")
    ) +
  guides(linetype = guide_legend("Temporal Orientation")) +
  labs(title = "Profile Frequencies", x = "Profile", y = "Proportion", fill = "Temporal Orientation") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = temp_ori_color) +
  cowplot::theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

profiles_freq_plot

### Parameters ----

# Inspect parameter estimates
pars_sel %>%
  select(temp_ori, task_type, par1:term2) %>%
  gather(par, value, par1:term2) %>%
  group_by(temp_ori, task_type, par) %>%
  summarize(mean = mean(value)) %>%
  spread(par, mean) %>%
  arrange(task_type) %>%
  knitr::kable(digits = 3)

# Define normalization ranges for each parameter
norm_range <- list(
  "par1" = c(-1, 0),
  "par2" = c(0, 1),
  "par3" = c(0, 1),
  "par4" = c(0, 1),
  "par6" = c(0, 1),
  "term1" = c(0, 1),
  "term2" = c(0, 1)
)

# Normalize parameter values
pars_norm <- normalize_param(pars_sel, norm_range)

#### Estimates ----

# Plot normalized parameter values
pars_plot <-
  pars_norm %>%
  select(temp_ori, task_type, par1:term2) %>%
  gather(par, value, par1:term2) %>%
  ggplot(aes(x = par, y = value, color = temp_ori)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(geom = "point", fun = mean, position = position_dodge(width = 0.5)) +
  stat_summary(
    geom = "errorbar",
    fun.data = mean_cl_normal,
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # annotate("rect", xmin = 6.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "royalblue") +
  # geom_vline(xintercept = 6.5, linetype = "dashed") +
  # annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "firebrick") +
  # geom_vline(xintercept = 4.5, linetype = "dashed") +
  # annotate("rect", xmin = 0, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "forestgreen") +
  annotate("text", label = "***", x = 4.9, y = 0.525) +
  labs(
    title = "Model Parameters",
    x = "Parameter", y = "Mean Value",
    color = "Temporal Orientation"
  ) +
  scale_x_discrete(
    limits = c("par1", "par3", "par2", "par4", "par5", "par6", "term1", "term2"), 
    labels = par_labels
    ) +
  coord_flip(ylim = c(-0.5, 0.5)) +
  scale_color_manual(values = temp_ori_color) +
  cowplot::theme_cowplot() +
  # facet_wrap(~task_type) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    # axis.text.x = element_text(angle = 90),
    # axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

pars_plot

# Plot parameters as function of weight omega
pars_weights_plot <-
  pars_norm %>%
  select(temp_ori, par5, term1, term2) %>%
  gather(term, value, -temp_ori, -par5) %>%
  ggplot(aes(x = par5, y = value, group = temp_ori, color = temp_ori)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "Terms as Function of Weights\n",
    x = expression(omega), y = "Value",
    color = "Temporal Orientation"
  ) +
  scale_color_manual(values = temp_ori_color) +
  cowplot::theme_cowplot() +
  facet_grid(~term) +
  theme(
    legend.position = "bottom",
    legend.justification = c(0.5, 0.5),
    plot.title = element_text(hjust = 0.5),
    # axis.text.x = element_text(angle = 90),
    # axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(color = "transparent", fill = "transparent")
  )

pars_weights_plot

#### Bootstrap test ----

boot_pars_file <- "output/models/boot/pars.rds"
if (!file.exists(boot_pars_file)) {
  # Define parameters to include in bootstrap analysis
  pars_cols <- c("par", "difference", "ci_low", "ci_high", "t", "p", "p_lab", "is_diff")
  par_names <- c(sprintf("par%i", 1:6), "term1", "term2")

  # Pre-allocate matrix for bootstrap results
  pars_boot <- as.data.frame(matrix(NA, ncol = length(pars_cols), nrow = length(par_names)))
  colnames(pars_boot) <- pars_cols
  
  # Initiate row index for allocating bootstrap results
  rowid <- 0

  for (par in par_names) {
    rowid <- rowid + 1
    
    # Subset parameter data for each condition
    d1 <- with(pars_norm, pars_norm[temp_ori == "Prospective", par])
    d2 <- with(pars_norm, pars_norm[temp_ori == "Retrospective", par])
    data <- c(d1, d2)
    
    # Bootstrap difference in means
    b <- boot(
      data, function(x, i) {
        x1 <- data[i[if (length(d1) > length(d2)) i <= length(d1) else i > length(d1)]]
        x2 <- data[i[if (length(d1) > length(d2)) i > length(d1) else i <= length(d1)]]
        ms <- sapply(list(x1, x2), mean)
        return(c(diff(ms)))
      }, nboot
      )
    
    # Compute 95% confidence intervals
    ci <- boot.ci(b, type = "bca")
    ci_low <- ci$bca[4]
    ci_high <- ci$bca[5]
    
    # Approximate t-test
    n <- length(b$t)
    diff <- mean(b$t)
    se <- sd(b$t) / sqrt(n)
    t = (diff - 0) / se
    p_val = 2 * pt(-abs(t), as.integer(n) - 1)
    p_lab <- as.character(cut(p_val, c(-Inf, 0.001, 0.01, 0.05, Inf), c(".001", ".01", ".05", "ns")))
    is_diff = if_else(ci_low <= 0 & ci_high >= 0, FALSE, TRUE)
    
    pars_boot[rowid, ] <- data.frame(par, diff, ci_low, ci_high, t, p_val, p_lab, is_diff)
  }
  
  save(pars_boot, file = boot_pars_file)
} else {
  load(file = boot_pars_file)
}

# Inspect parameter differences
knitr::kable(pars_boot, digits = 3)

# Results -----------------------------------------------------------------

# Model comparisons

# Create legend for model frequencies plot
relf_legend <-
  expand.grid(
    temp_ori = c("Prospective", "Retrospective"),
    color = temp_ori_color,
    rank = 1:n_ranks
  ) %>%
  mutate(alpha = seq(1, 0.25, length.out = n_ranks)[rank]) %>%
  ggplot(aes(x = rank, y = temp_ori, fill = temp_ori, alpha = alpha)) +
  coord_equal() +
  geom_tile(linewidth = 0.5, color = "white") +
  scale_x_discrete(limits = sprintf("R%i", 1:n_ranks), position = "top") +
  scale_y_discrete(limits = c("Retrospective", "Prospective"), position = "left") +
  scale_fill_manual(values = temp_ori_color) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(hjust = 0.5),
    axis.text.y = element_text(hjust = 1)
  )

relf_plot_legend <- # inset legend to plot
  relf_plot +
  inset_element(
    relf_legend,
    left = 0.25, bottom = 0.75, right = 0.75, top = 1.075,
    ignore_tag = FALSE
  )

p3a <- loss_plot
p3b <- loss_test_plot
p3c <- relf_plot_legend + theme(legend.position = "none")
p3d <- pcor_plot + theme(legend.position = "none")

fig_comparisons <-
  (p3a + p3b + p3c + p3d) +
    plot_annotation(tag_level = list(c("a", "b", "c", "", "d"), "1")) &
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

fig_comparisons
cowplot::ggsave2("output/plots/Fig4.pdf", fig_comparisons, width = 10, height = 7, scale = 1.2)

# Predictions

p4a <- pars_plot + theme(legend.position = "none")
p4b <- profiles_plot
p4c <- profiles_freq_plot

fig_preds <-
  (p4a + p4b + p4c) +
    plot_layout(nrow = 1, widths = c(1, 1, 1.5)) +
    plot_annotation(tag_levels = "a") &
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

fig_preds
cowplot::ggsave2("output/plots/Fig5.pdf", fig_preds, width = 10, height = 3.25, scale = 1.2)
