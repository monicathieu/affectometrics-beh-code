## setup ----

require(dplyr)

## function definition ----

count_trials_seen <- function (df, max_trials_seen) {
  df %>%
    count(subj_num, block) %>%
    # Strict filter: must exact match the max
    filter(n %in% max_trials_seen) %>%
    # unfortunately, can't currently control the name of the count col
    count(subj_num) %>% 
    rename(n_complete_blocks = n) %>% 
    return()
}

count_resps <- function (df) {
  df %>%
    group_by(subj_num, block) %>%
    summarize(n_trials = n(),
              n_timeouts = sum(timeout, na.rm = TRUE)) %>%
    mutate(n_trials_responded = n_trials - n_timeouts) %>%
    select(-n_trials, -n_timeouts) %>% 
    return()
}

count_timeouts <- function (df, cutoff_1, cutoff_2) {
  df %>%
    group_by(subj_num, block) %>%
    summarize(n_timeouts = sum(timeout, na.rm = TRUE)) %>%
    group_by(subj_num) %>%
    summarize(timeouts_1block = any(n_timeouts >= cutoff_1),
              timeouts_2block = sum(n_timeouts >= cutoff_2) >= 2) %>% 
    return()
}

get_rt_quantiles <-  function (df) {
  df %>%
    # only for categorical trials rn
    filter(resp_type == "cat", !is.na(resp)) %>%
    select(-resp_type) %>%
    group_by(subj_num) %>%
    summarize(rt_75 = quantile(rt, .75)) %>%
    mutate(rt_quantile_75 = c(scale(rt_75))) %>% 
    return()
}
