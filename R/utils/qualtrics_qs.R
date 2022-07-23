## @knitr preloading ----
require(tidyverse)
require(magrittr)
require(rlang)
source(here::here("R", "utils", "qualtrics_helpers.R"))

## @knitr demographics ----

# requires the LABEL data!
get_senpai_qualtrics_demos <- function(df) {
  ses <- df %>%
    select(subj_num, starts_with("ses")) %>%
    mutate_all(na_if, y = "Off") %>%
    gather(ses, delete, -subj_num) %>%
    filter(!is.na(delete)) %>%
    mutate(ses = as.numeric(str_sub(ses, start = 5L)) - 6) %>%
    select(-delete)
  
  race <- df %>%
    select(subj_num, starts_with("race")) %>%
    gather(delete, race, -subj_num) %>%
    filter(!is.na(race)) %>%
    group_by(subj_num) %>%
    mutate(n_races = n(),
           race_summary = if_else(n_races > 1, "More than one race", race)) %>%
    distinct(subj_num, race_summary)
  
  if ("handedness" %in% names(df)) {
    demos <- df %>%
      select(subj_num, age, handedness, gender, starts_with("race"), ethnicity, edu, starts_with("dx")) %>%
      mutate(handedness = str_sub(handedness, start = 11L)) %>%
      left_join(ses, by = "subj_num") %>%
      left_join(race, by = "subj_num")
  } else {
    demos <- df %>%
      select(subj_num, age, gender, starts_with("race"), ethnicity, edu, starts_with("dx")) %>%
      left_join(ses, by = "subj_num") %>%
      left_join(race, by = "subj_num")
  }
  
  return (demos)
}

## @knitr counting senpai no responses ----

# also requires label data (all columns render in this one)
get_senpai_qualtrics_nas <- function (df) {
  out <- df %>%
    select(subj_num:dx_med) %>%
    group_by(subj_num) %>%
    do(nas_i_panas_sf = apply(select(., starts_with("i_panas_sf")), 1, function(x) sum(is.na(x))),
       nas_sns = apply(select(., starts_with("sns")), 1, function(x) sum(is.na(x))),
       nas_bpq = apply(select(., starts_with("bpq")), 1, function(x) sum(is.na(x))),
       nas_tas20 = apply(select(., starts_with("tas20")), 1, function(x) sum(is.na(x))),
       nas_ders = apply(select(., starts_with("ders")), 1, function(x) sum(is.na(x)))) %>%
    mutate_at(vars(starts_with("nas")), unlist) %>%
    ungroup() %>%
    mutate(nas_total = rowSums(.[, -1]))
  
  return (out)
}

## @knitr i_panas_sf ----
# I-PANAS-SF (Thompson, 2007)
# Scored where the "very slightly/not at all" is 1, and "always" is 5 (same as Qualtrics default coding)
# True codings (I messed it up when creating the survey at first):
# items 1, 2, 4, 6, 9 are NEG
# thus, items 3, 5, 7, 8, 10 are POS

get_senpai_qualtrics_i_panas_sf <- function(df, type = "likert") {
  
  out <- df %>%
    grab_q_cols("i_panas_sf") %>% 
    # pivot_items_legacy expects underscores to only be delimiting between name sections
    rename_with(~str_replace_all(.x, "i_panas_sf", "ipanassf"), .cols = contains("i_panas_sf")) %>% 
    pivot_items_legacy(q_name = "ipanassf", type = type, n_likerts = 5, min_likert = 1) %>% 
    mutate(response_patched = coalesce(response, 1)) %>% 
    mutate(subscale = if_else(item %in% c(3,5,7,8,10), "pos", "neg")) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>%
    underscore_subscales("i_panas_sf") %>%
    pivot_subscales()

  return (out)
  
}

## @knitr bpq ----
# BPQ (Poreh et al, 2006)
# Scored by summing the TRUE/positive coded responses.
# NOT ACTIVELY MAINTAINED as we are not administering this when the DERS is available as of 2019

get_senpai_qualtrics_bpq <- function (df) {
  bpq_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "bpq_raw.csv")) %>%
    separate(question, into = c("q_num", "q_text"), extra = "merge") %>%
    mutate(q_num = as.numeric(q_num),
           reverse = if_else(endsWith(q_text, "*"), TRUE, FALSE))
  
  out <- df %>%
    select(subj_num, starts_with("bpq")) %>%
    gather("item", "response", -subj_num) %>%
    mutate(item = as.numeric(str_sub(item, start = 6L))) %>%
    left_join(select(bpq_qs, -q_text), by = c("item" = "q_num")) %>%
    mutate(response = if_else(reverse,
                              recode(response, "1" = FALSE, "2" = TRUE),
                              recode(response, "1" = TRUE, "2" = FALSE))) %>%
    group_by(subj_num, subscale) %>%
    summarize(score = sum(response, na.rm = TRUE),
              n_items_responded = sum(!is.na(response)),
              n_items_seen = length(response)) %>%
    mutate(subscale = tolower(str_replace_all(subscale, "[ -/]", "_")),
           subscale = paste0("bpq_", subscale),
           score_adj = (score / n_items_responded) * n_items_seen) %>%
    select(-score, -n_items_responded, -n_items_seen) %>%
    spread(subscale, score_adj)
  
  return (out)
}


## @knitr tas20 ----
# TAS-20 (Bagby, Parker, & Taylor 1994)
# Scored by summing the responses, where 1 is lowest and 5 is highest (same as Qualtrics default)

get_senpai_qualtrics_tas20 <- function (df, type = "likert") {
  tas20_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "tas20_raw.csv"))
  
  out <- df %>%
    grab_q_cols("tas20") %>% 
    pivot_items_legacy(q_name = "tas20", type = type, n_likerts = 5, min_likert = 1) %>% 
    left_join(select(tas20_qs, -q.text), by = c("item" = "q.number")) %>%
    mutate(response = if_else(reverse, ((-1) * (response-3) + 3), response),
           response_patched = coalesce(response, 3)) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>% 
    underscore_subscales("tas20") %>% 
    pivot_subscales()
  
  return (out)
}

## @knitr ders ----
# DERS (Gratz & Roemer, 2004)
# Scored by summing the responses, where 1 is least often and 5 is most often
# (same as Qualtrics default)

get_senpai_qualtrics_ders <- function (df, type = "likert") {
  ders_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "ders_raw.csv")) %>%
    rename(q_num = q.num, q_text = q.text)
  
  out <- df %>%
    grab_q_cols("ders") %>% 
    pivot_items_legacy(q_name = "ders", type = type, n_likerts = 5, min_likert = 1) %>%
    left_join(select(ders_qs, -q_text), by = c("item" = "q_num")) %>% 
    mutate(response = if_else(reverse, ((-1) * (response-3) + 3), response),
           response_patched = coalesce(response, 0)) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>% 
    underscore_subscales("ders") %>% 
    pivot_subscales()
  
  return (out)
}

## @knitr als18 ----
# ALS 18-item short form (Oliver & Simons, 2004)
# nothing is reverse coded!
# scaled from 0 (minimum, doesn't describe me at all or "very undescriptive")
# to 3 (maximum, "very descriptive")
# appears to be scored by summing responses

get_senpai_qualtrics_als18 <- function (df) {
  als18_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "als18_raw.csv"))
  
  out <- df %>%
    grab_q_cols("als18") %>% 
    pivot_items_legacy(type = "slider", n_likerts = 4, min_likert = 0) %>% 
    mutate(response_patched = coalesce(response, 0)) %>%
    left_join(select(als18_qs, q.number, subscale = subscale.6), by = c("item" = "q.number")) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>% 
    underscore_subscales("als18") %>% 
    pivot_subscales()
  
  return (out)
}

## @knitr beq ----
# scaled from 1 (strongly disagree) - 7 (strongly agree)
# based on the scoring on the PDF version of the survey on james gross' lab website,
# scores for subscales are calculated by AVERAGING, not SUMMING
# then the overall score is an average where EACH SUBSCALE IS EQUALLY WEIGHTED
# technically don't need to adjust scores for missing responses,
# because scores are already means, which effectively impute if na.rm = TRUE

get_senpai_qualtrics_beq <- function (df) {
  beq_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "beq_raw.csv"))
  
  out <- df %>%
    grab_q_cols("beq") %>% 
    pivot_items_legacy(type = "slider", n_likerts = 7, min_likert = 1) %>% 
    left_join(select(beq_qs, q.number, subscale, reverse), by = c("item" = "q.number")) %>%
    mutate(response = if_else(reverse, 4 - (response - 4), response),
           response_patched = coalesce(response, 4)) %>%
    group_by(subj_num, subscale) %>%
    summarize_score(FUN = mean) %>% 
    underscore_subscales("beq") %>% 
    pivot_subscales()
  
  return (out)
}

## @knitr cesdr ----
# if nearly every day for 2 weeks is YES, then score is 4,
# otherwise we can say response is scaled so that 7 days a week = 3
# and 0 days a week = 0
# The way this is currently written, it will throw a warning from pivot_longer
# bc the cesdr2 questions do not have that _1 tag on the end of the name

get_senpai_qualtrics_cesdr <- function (df) {
  cesdr_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "cesdr_raw.csv"))
  
  out <- df %>%
    grab_q_cols("cesdr") %>%
    pivot_longer(-subj_num,
                 names_to = c("item", ".value", NA),
                 values_to = "response",
                 names_sep = "_",
                 names_transform = list(item = as.integer),
                 values_transform = list(cesdr1 = as.integer,
                                      cesdr2 = as.integer)) %>%
    mutate(# recode "no" to the 2 weeks question as 0
           cesdr2 = recode(cesdr2, `2` = 0L),
           # scaling things from days of the week space back to 0-4 scoring space
           response = if_else(cesdr2 == 1, 4, cesdr1 * (3/7), missing = cesdr1 * (3/7)),
           response_patched = coalesce(response, 0)) %>%
    left_join(select(cesdr_qs, q.number, subscale), by = c("item" = "q.number")) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>%
    underscore_subscales("cesdr") %>%
    pivot_subscales()
  
  return (out)
}

## @knitr sticsa ----

# Not 100% clear from Gros et al 2007 how I should score, but I'm guessing it's
# 1 (not at all, minimum) to 4 (very much so, maximum)
# and then summing the responses

get_senpai_qualtrics_sticsa <- function (df) {
  
  sticsa_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "sticsa_raw.csv"))
  
  out <- df %>%
    grab_q_cols("sticsa") %>%
    pivot_longer(-subj_num,
                 names_to = c("item", NA, "q_type", NA),
                 values_to = "response",
                 names_sep = "_",
                 names_transform = list(item = as.integer,
                                     q_type = as.character)) %>%
    mutate(response = (response * .03) + 1,
           response_patched = coalesce(response, 1)) %>%
    left_join(select(sticsa_qs, q.number, subscale), by = c("item" = "q.number")) %>%
    group_by(subj_num, q_type, subscale) %>%
    summarize_score() %>%
    ungroup() %>%
    underscore_subscales("sticsa") %>%
    select(-q_type) %>%
    pivot_subscales()
  
  return (out)
}
## @knitr swls ----
# based on diener et al 1985, scored as a 1 (strongly disagree) - 7 (strongly agree) likert
# score is sum. no subscales, after all it's 5 questions

get_senpai_qualtrics_swls <- function (df) {
  
  out <- df %>%
    grab_q_cols("swls") %>%
    pivot_items_legacy(type = "slider", n_likerts = 7, min_likert = 1) %>%
    mutate(response_patched = coalesce(response, 4)) %>%
    group_by(subj_num) %>%
    summarize_score() %>% 
    select(subj_num, adj.swls = score_adj, patched.swls = score_patched)
  
  return (out)
}

## @knitr irq ----

# now if I am correct in understanding qualtrics, it still records loop and merge order
# in the order that the fields are entered, which means I shouldn't have to do
# any number shuffling to get the right item numbers
# also scored as a 1 (strongly disagree) - 7 (strongly agree) likert
# and summed, per craig's github

get_senpai_qualtrics_irq <- function (df) {
  irq_qs <- read_csv(here::here("ignore", "stim_info", "questionnaire_templates", "irq_raw.csv"))
  
  out <- df %>%
    grab_q_cols("irq") %>% 
    select(-irqItemOrder) %>% 
    pivot_items_legacy(type = "slider", n_likerts = 7, min_likert = 1) %>% 
    mutate(response_patched = coalesce(response, 4)) %>%
    left_join(select(irq_qs, q.number, subscale), by = c("item" = "q.number")) %>%
    group_by(subj_num, subscale) %>%
    summarize_score() %>% 
    underscore_subscales("irq") %>% 
    pivot_subscales()
  
  return (out)
}

## @knitr followup NA counting ----
get_senpai_qualtrics_followup_nas <- function (df) {
  out <- df %>%
    select(subj_num,
           starts_with("als18"),
           starts_with("beq"),
           starts_with("cesdr1"),
           starts_with("sticsa"),
           starts_with("swls"),
           starts_with("irq_")) %>%
    filter(!is.na(subj_num)) %>%
    group_by(subj_num) %>%
    do(nas_als18 = apply(select(., starts_with("als18")), 1, function(x) sum(is.na(x))),
       nas_beq = apply(select(., starts_with("beq")), 1, function(x) sum(is.na(x))),
       nas_cesdr = apply(select(., starts_with("cesdr")), 1, function(x) sum(is.na(x))),
       nas_sticsa_state = apply(select(., starts_with("sticsa_state")), 1, function(x) sum(is.na(x))),
       nas_sticsa_trait = apply(select(., starts_with("sticsa_trait")), 1, function(x) sum(is.na(x))),
       nas_swls = apply(select(., starts_with("swls")), 1, function(x) sum(is.na(x))),
       nas_irq = apply(select(., starts_with("irq")), 1, function(x) sum(is.na(x)))) %>%
    mutate_at(vars(starts_with("nas")), unlist) %>%
    ungroup() %>%
    mutate(nas_total = rowSums(.[, -1])) %>%
    select(subj_num, nas_total, everything())
  
  return (out)
}
