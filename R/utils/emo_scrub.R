## setup ----

require(tidyverse)
require(rlang)
source(here::here("R", "utils", "scrub_helpers.R"))

## calc scrubs ----

id_bad_subjs <- function (task, qs) {

  less_raw_all <- task %>%
    left_join(qs %>%
                select(subj_num, expt_type, age),
              by = c("subj_num", "expt_type"))
  
  # scrubbing people for whom we lost some of the trial data. for whatever reason
  scrubs <- less_raw_all %>%
    nest(data = everything()) %>%
    mutate(ages = map(data, ~distinct(.x, subj_num, expt_type, age)),
           trials_seen = map(data, count_trials_seen, max_trials_seen = c(43, 54)),
           resp_counts = map(data, count_resps),
           # I have decided that I shall use the following cutoffs
           # exclude all subjects who have either:
           # >= 10 NRs in at least ONE block
           # OR >= 5 NRs in at least TWO blocks
           enough_resps = map(data, count_timeouts, cutoff_1 = 10, cutoff_2 = 5),
           rt_quantiles = map(data, get_rt_quantiles),
           slider_not_lazy = map(data, ~.x %>%
                                   filter(resp_type == "cont", !is.na(resp)) %>%
                                   select(-resp_type) %>% 
                                   group_by(subj_num, block) %>%
                                   summarize(n_0s = sum(resp == 0),
                                             n_100s = sum(resp == 1),
                                             n_lazy = sum(abs(resp - slider_start) < .01)) %>%
                                   summarize(n_0s = round(mean(n_0s), 1),
                                             n_100s = round(mean(n_100s), 1),
                                             n_lazy = round(mean(n_lazy), 1)) %>%
                                   arrange(desc(n_lazy)))) %>%
    # so this doesn't occupy as much space in memory
    select(-data) %>% 
    # this whole rigamarole is required to de-list-col the sub-dfs for reduce to behave
    pivot_longer(cols = everything(), names_to = "delete", values_to = "data") %>% 
    pull(data) %>% 
    reduce(full_join, by = "subj_num") %>% 
    # so that the scrubbing criteria are specified 1x in the code
    mutate(enough_trials_seen = n_complete_blocks == 6,
           enough_resps = !(timeouts_1block | timeouts_2block),
           not_too_fast = coalesce(rt_75 > 500, FALSE),
           # semi-arbitrary cutoff for having too many responses without moving the slider off of the start point
           slider_not_lazy = n_lazy < 20,
           age_valid = age <= 35)
  
  return (scrubs)
}

## bind scrub info onto data ---- 

scrub_task <- function (task, qs, scrubs) {
  
  task_scrubbed <- task %>%
    left_join(qs %>%
                select(subj_num, expt_type, age),
              by = c("subj_num", "expt_type")) %>% 
    nest(data = -c(subj_num, expt_type, age, block)) %>%
    left_join(scrubs, by = c("subj_num", "expt_type", "age", "block")) %>% 
    filter(enough_trials_seen, enough_resps, not_too_fast, slider_not_lazy, age_valid) %>% 
    select(subj_num, expt_type, age, block, data, n_trials_responded, rt_quantile_75) %>% 
    unnest(data) %>%
    mutate(emo_block = recode(emo_block, neutral_amusement = "0 to +",
                              neutral_positive = "0 to +",
                              neutral_fear = "0 to -",
                              neutral_negative = "0 to -",
                              amusement_fear = "+ to -",
                              positive_negative = "+ to -"),
           emo_block = factor(emo_block, levels = c("0 to +", "0 to -", "+ to -")),
           n_trials_responded = if_else(emo_block == "+ to -",
                                        -0.1 * (n_trials_responded - 54L),
                                        -0.1 * (n_trials_responded - 43L))) %>%
    select(-age)
  
  return (task_scrubbed)
  # write_csv(d_all, paste0(path, "/task_goodsubs.csv"))
}

## write out ----

get_bad_subjs <- function (scrubs) {
  bad_subjs <- scrubs %>% 
    distinct(subj_num,
             expt_type,
             age,
             n_complete_blocks,
             timeouts_1block, timeouts_2block,
             rt_75,
             n_lazy,
             enough_trials_seen, enough_resps, not_too_fast, slider_not_lazy, age_valid)
  
  return (bad_subjs)
  
  # write_csv(paste0(path, "/task_scrubs.csv"))
}
