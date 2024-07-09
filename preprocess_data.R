# Title: Data preprocessing for effort and value study
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-03-07
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Load packages
library(tidyverse)
library(readxl)

# Combine individual experiment data files
experiment_combined <-
  tibble(
    file_name = fs::dir_ls(
      "data/base/experiment/",
      type = "file", recurse = TRUE, 
      regexp = "/\\d{1}_data.xlsx"
      )
    ) %>%
  mutate(file_content = map(file_name, ~read_excel(file.path(.), col_types = "text"))) %>%
  unnest(file_content) %>%
  select(1, 3, 4, 5, 6, 7, 8, 11, 19) %>%
  purrr::set_names(nm = c(
    "file_name", "resp_type", "trial_num",
    "trial_step", "X1", "resp", "rt", "ord_org",
    "effort_level"
  )) %>%
  group_by(file_name) %>%
  mutate(id = sapply(str_split(file_name, "/"), function(x) x[4])) %>%
  group_by(id) %>%
  mutate(
    session = str_extract(file_name, "\\d{1}(?=_data.xlsx)"),
    condition = sapply(str_split(file_name, "/"), function(x) paste0(x[5], x[6])),
    task_type = case_when(
      str_detect(condition, "H") ~ "Hypothetical",
      str_detect(condition, "R") ~ "Real"
    ),
    temp_ori = case_when(
      str_detect(condition, "C") ~ "Prospective",
      str_detect(condition, "I") ~ "Retrospective"
    ),
    condition = if_else(
      length(unique(condition)) == 1, 
      paste0(rep(unique(condition), 2), collapse = ""), 
      paste(unique(condition), collapse = "")
      ),
    id = paste0(id, condition, session),
    effort_level = str_replace(effort_level, "w prezencie", "0"),
    effort_level = str_extract(effort_level, "\\d+"),
    resp_type = recode(
      resp_type,
      "pica pica" = "choice",
      "bubo bubo" = "post",
      "pica pica time travel" = "time_travel"
    ),
    resp_type = case_when(
      is.na(trial_step) ~ "indiff", 
      resp_type == "post" & trial_step == 1 ~ "payoff_liking",
      resp_type == "post" & trial_step == 2 ~ "item_liking",
      resp_type == "post" & trial_step == 3 ~ "choice_certain",
      T ~ resp_type
      ),
    resp = as.numeric(
      case_when(
        resp_type %in% c("payoff_liking", "item_liking", "choice_certain") ~ resp,
        resp_type == "choice" & resp == "zmienna" ~ "1", 
        resp_type == "choice" & resp == "stala" ~ "0",
        resp_type == "indiff" ~ X1
        )),
    E1 = 0,
    X2 = 15,
    E2 = effort_level,
    across(c(rt, X1, E1, X2, E2), as.numeric),
    outcome = if_else(resp == 0, X1, X2),
    alternative = if_else(resp == 0, X2, X1)
  ) %>%
  fill(E2) %>%
  ungroup() %>%
  filter(!resp_type == "time_travel") %>%
  select(
    id, session, task_type, temp_ori,
    trial = trial_num, X1, E1, X2, E2, resp_type, resp, rt,
    outcome, alternative
  )

# Combine individual participant info files
participant_info <-
  tibble(
    file_name = fs::dir_ls(
      "data/base/experiment/", 
      type = "file", recurse = TRUE, 
      regexp = "/\\d{1}_data_czas.xlsx"
    )
  ) %>%
  mutate(file_content = map(file_name, ~read_excel(file.path(.), col_types = "text"))) %>%
  unnest(file_content) %>%
  ungroup() %>%
  mutate(id = sapply(str_split(file_name, "/"), function(x) x[4])) %>%
  group_by(id) %>%
  mutate(
    sex = case_when(plec %in% c("1", "M") ~ "Male", plec %in% c("2", "K", "2-K") ~ "Female"),
    age = 2019 - as.numeric(rok_urodzenia),
    session = str_extract(file_name, "\\d{1}(?=_data_czas.xlsx)"),
    condition = sapply(str_split(file_name, "/"), function(x) paste0(x[5], x[6])),
    task_type = case_when(
      str_detect(condition, "H") ~ "Hypothetical",
      str_detect(condition, "R") ~ "Real"
    ),
    temp_ori = case_when(
      str_detect(condition, "C") ~ "Prospective",
      str_detect(condition, "I") ~ "Retrospective"
    )
  ) %>%
  ungroup() %>%
  left_join(mutate(read_csv("data/performance.csv"), sex = str_to_sentence(sex))) %>%
  group_by(id) %>%
  mutate(
    condition = if_else(
      length(unique(condition)) == 1, 
      paste0(rep(unique(condition), 2), collapse = ""), 
      paste(unique(condition), collapse = "")
    ),
    id = paste0(id, condition, session),
    perf_time = if_else(task_type == "Hypothetical", 0, perf_time)
    ) %>%
  ungroup() %>%
  select(id, sex, age, group, session, task_type, temp_ori, perf_time)

# Prepare choice data
experiment_choice <- 
  experiment_combined %>%
  filter(
    resp_type == "choice",
    id %in% participant_info$id
    ) %>%
  mutate(EffortfulOptionChosen = as.integer(resp)) %>%
  select(
    id, session, task_type, temp_ori,
    trial, X1, E1, X2, E2, EffortfulOptionChosen, rt,
    outcome, alternative
    ) %>%
  arrange(id, session, trial) %>%
  filter(E2 > 0, X1 > 0) %>%
  select(id, session, task_type, temp_ori, trial, X1, E1, X2, E2, EffortfulOptionChosen, rt)

# Prepare choice evaluation data
experiment_last_choice <- 
  experiment_combined %>%
  select(id, session, task_type, temp_ori, trial, X2, E2, resp_type, resp) %>%
  filter(
    !resp_type == "choice",
    id %in% participant_info$id
  ) %>%
  spread(resp_type, resp) %>%
  arrange(id, session, trial, E2) %>%
  select(id, session, task_type, temp_ori, trial, X2, E2, indiff, choice_certain, item_liking, payoff_liking)

# Check for NAs
sum(is.na(participant_info))
sum(is.na(experiment_choice))
sum(is.na(experiment_last_choice))

# Check counts
n_distinct(str_extract(participant_info$id, ".+(?=_)"))
n_distinct(str_extract(experiment_choice$id, ".+(?=_)"))
n_distinct(str_extract(experiment_last_choice$id, ".+(?=_)"))

# Inspect sample composition
participant_info %>% 
  group_by(id = str_extract(id, "\\w{2}\\d{1,2}(?=H)")) %>%
  slice(1) %>%
  group_by(group) %>% 
  summarise(n_distinct(id))

participant_info %>% 
  group_by(id = str_extract(id, "\\w{2}\\d{1,2}(?=H)")) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(
    n = n_distinct(id),
    mean_age = mean(age),
    sd_age = sd(age)
  )

participant_info %>% 
  group_by(id = str_extract(id, "\\w{2}\\d{1,2}(?=H)")) %>%
  slice(1) %>%
  group_by(sex) %>%
  summarise(
    n = n_distinct(id),
    mean_age = mean(age),
    sd_age = sd(age)
  )

# Save prepared data files
write.table(participant_info, "data/processed/participants.txt")
write.table(experiment_choice, "data/processed/choice.txt")
write.table(experiment_last_choice, "data/processed/last_choice.txt")
