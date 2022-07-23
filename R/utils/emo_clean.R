## setup ----
require(tidyverse)
require(magrittr)

## load from RAW: existing stimulus levels ----
# smart (hopefully) scaling the stimulus levels to be between 0 and 1
info <- read_csv(here::here("ignore", "stim_info", "amuse_fear_gif_info.csv")) %>%
  mutate(amuse = amuse / 5,
         fear = fear / 5,
         bipolar = (bipolar + 2)/4)

## load from RAW: round 1 amuse fear ----

clean_task_af <- function (paths) {
  
  af_raw <- paths %>%
    map(~read_csv(.)) %>%
    map(~mutate(., `Timed Out` = as.integer(`Timed Out`))) %>%
    bind_rows()
  
  af_less_raw <- af_raw %>%
    filter(randomise_trials %in% 1:6) %>%
    select(subj_num = `Participant Public ID`,
           starts_with("counterbalance"),
           trial_num = `Trial Number`,
           trial_screen = `Screen Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           timeout = `Timed Out`,
           cb_order = `Spreadsheet Name`,
           resp_type = display,
           prompt_l = promptLText,
           prompt_r = promptRText,
           starts_with("fname")) %>%
    filter(trial_screen == "gif") %>%
    mutate(rt = rt - 5000, # centering so the minimum rt is 0
           rt_secs = rt / 1000,
           resp_type = recode(resp_type, trialCat = "cat", trialCont = "cont"),
           timeout = as.numeric(timeout),
           list_version = coalesce(`counterbalance-hhri`,
                                   `counterbalance-ib52`,
                                   `counterbalance-57z1`,
                                   `counterbalance-4r2c`),
           fname_seen = case_when(list_version == "fnameV1" ~ fnameV1,
                                  list_version == "fnameV2" ~ fnameV2,
                                  list_version == "fnameV3" ~ fnameV3,
                                  list_version == "fnameV4" ~ fnameV4,
                                  list_version == "fnameV5" ~ fnameV5),
           timeout = if_else(resp_type == "cont", lead(timeout, 1), timeout),
           cb_resp_order = if_else(endsWith(cb_order, "CatFirst"), "cat_first", "cont_first")) %>%
    unite(block, resp_type, prompt_l, prompt_r, remove = FALSE) %>%
    unite(emo_block, prompt_l, prompt_r, remove = FALSE) %>%
    mutate(block = tolower(block),
           emo_block = tolower(emo_block),
           resp_label = resp,
           resp = case_when(resp_label == "NEUTRAL" ~ 0L,
                            resp_label == "FEAR" ~ 1L,
                            block == "cat_neutral_amusement" & resp_label == "AMUSEMENT" ~ 1L,
                            block == "cat_amusement_fear" & resp_label == "AMUSEMENT" ~ 0L,
                            TRUE ~ as.integer(resp))) %>%
    filter(!is.na(resp)) %>% # removing the "submitResp" cont rows, AFTER timeout has been recorded to the slider row
    select(-starts_with("counterbalance"), -starts_with("fnameV")) %>%
    left_join(info, by = c("fname_seen" = "fname")) %>%
    mutate(stim_level = case_when(block %in% c("cat_neutral_amusement", "cont_neutral_amusement") ~ amuse,
                                  block %in% c("cat_neutral_fear", "cont_neutral_fear") ~ fear,
                                  block %in% c("cat_amusement_fear", "cont_amusement_fear") ~ bipolar,
                                  TRUE ~ NA_real_),
           resp = na_if(resp, 999), # when no button was pressed in the categorical condition
           resp = if_else(resp_type == "cont", resp / 100, as.double(resp)),
           resp = if_else(resp == 0.5 & timeout == 1, NA_real_, resp, missing = resp),
           slider_start = 0.5,
           expt_type = "amusement_fear")
  
  return (af_less_raw)
}

## load from RAW: round 2 pos neg ----

clean_task_pn <- function (paths) {

  pn_raw <- paths %>%
    map(~read_csv(.)) %>%
    map(~mutate(., `Timed Out` = as.integer(`Timed Out`))) %>%
    bind_rows() %>% 
    mutate(expt_type = "positive_negative")
  
  pn_less_raw <- pn_raw %>%
    filter(randomise_trials %in% 1:6) %>%
    select(subj_num = `Participant Public ID`,
           expt_type,
           starts_with("counterbalance"),
           trial_num = `Trial Number`,
           trial_screen = `Screen Name`,
           rt = `Reaction Time`,
           resp = `Response`,
           timeout = `Timed Out`,
           cb_order = `Spreadsheet Name`,
           resp_type = display,
           prompt_l = promptLText,
           prompt_r = promptRText,
           slider_start = sliderStart,
           starts_with("fname")) %>%
    filter(trial_screen == "gif") %>%
    mutate(rt = rt - 5000, # centering so the minimum rt is 0
           rt_secs = rt / 1000,
           resp_type = recode(resp_type, trialCat = "cat", trialCont = "cont"),
           list_version = coalesce(`counterbalance-hhri`,
                                   `counterbalance-ib52`,
                                   `counterbalance-57z1`,
                                   `counterbalance-4r2c`),
           fname_seen = case_when(list_version == "fnameV1" ~ fnameV1,
                                  list_version == "fnameV2" ~ fnameV2,
                                  list_version == "fnameV3" ~ fnameV3,
                                  list_version == "fnameV4" ~ fnameV4,
                                  list_version == "fnameV5" ~ fnameV5),
           timeout = if_else(resp_type == "cont", lead(timeout, 1), timeout),
           cb_resp_order = if_else(endsWith(cb_order, "CatFirst"), "cat_first", "cont_first")) %>%
    unite(block, resp_type, prompt_l, prompt_r, remove = FALSE) %>%
    unite(emo_block, prompt_l, prompt_r, remove = FALSE) %>%
    mutate(block = tolower(block),
           emo_block = tolower(emo_block),
           resp_label = resp,
           resp = case_when(resp_label == "NEUTRAL" ~ 0L,
                            resp_label == "NEGATIVE" ~ 1L,
                            block == "cat_neutral_positive" & resp_label == "POSITIVE" ~ 1L,
                            block == "cat_positive_negative" & resp_label == "POSITIVE" ~ 0L,
                            TRUE ~ as.integer(resp))) %>%
    filter(!is.na(resp)) %>% # removing the "submitResp" cont rows, AFTER timeout has been recorded to the slider row
    select(-starts_with("counterbalance"), -starts_with("fnameV")) %>%
    left_join(info, by = c("fname_seen" = "fname")) %>%
    mutate(stim_level = case_when(block %in% c("cat_neutral_positive", "cont_neutral_positive") ~ amuse,
                                  block %in% c("cat_neutral_negative", "cont_neutral_negative") ~ fear,
                                  block %in% c("cat_positive_negative", "cont_positive_negative") ~ bipolar,
                                  TRUE ~ NA_real_),
           resp = na_if(resp, 999), # when no button was pressed in the categorical condition
           resp = if_else(resp_type == "cont", resp / 100, as.double(resp)),
           resp = if_else(resp == 0.5 & timeout == 1, NA_real_, resp, missing = resp),
           slider_start = slider_start / 100)
  
  return (pn_less_raw)
  
}

## load from RAW: combine two ----

clean_task_combine <- function (af, pn) {

  less_raw <- bind_rows(af, pn) %>%
    select(-trial_screen) %>%
    distinct(subj_num, block, fname_seen, .keep_all = T)
  
  return (less_raw)
}

# caching to load from cache in Rmd if you want
# write_csv(less_raw, here::here("ignore", "data_gdrive", "emo", "online", "task_allsubs.csv"))
