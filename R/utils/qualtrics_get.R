## setup ----

require(tidyverse)
require(magrittr)
require(qualtRics)
require(rlang)
source(here::here("R", "utils", "qualtrics_qs.R"))

## senpai online beh huge wrapper definition ----

get_senpai_qualtrics <- function(survey_path_numeric, survey_path_choicetext, out_file = NULL) {
  
  subj_id_var = "prolific_id"
  
  d_raw <- read_survey(survey_path_numeric) %>% 
    rename(subj_num = !!subj_id_var) %>%
    # they are ordered most to least recent, so this removes the EARLIER responses, which should be less complete
    filter(!duplicated(subj_num)) %>%
    # repairing people who didn't put their prolific ID in the box :(
    mutate(if_else(subj_num == "rchensd1", "57c7ba183c74450001a367d3", subj_num)) %>% 
    mutate(across(where(is.numeric), na_if, -99),
           across(where(is.character), na_if, "-99"))
  
  d_raw_label <- read_survey(survey_path_choicetext) %>%
    rename(subj_num = !!subj_id_var) %>%
    filter(!duplicated(subj_num)) %>%
    mutate(if_else(subj_num == "rchensd1", "57c7ba183c74450001a367d3", subj_num)) %>% 
    mutate(across(where(is.numeric), na_if, -99),
           across(where(is.character), na_if, "-99"))
  
  demos <- get_senpai_qualtrics_demos(d_raw_label)
  nas <- get_senpai_qualtrics_nas(d_raw_label)
  i_panas_sf_total <- get_senpai_qualtrics_i_panas_sf(d_raw)
  bpq_total <- get_senpai_qualtrics_bpq(d_raw)
  tas20_total <- get_senpai_qualtrics_tas20(d_raw)
  ders_total <- get_senpai_qualtrics_ders(d_raw)
  
  all <- demos %>%
    left_join(nas, by = "subj_num") %>%
    left_join(i_panas_sf_total, by = "subj_num") %>%
    left_join(bpq_total, by = "subj_num") %>%
    left_join(tas20_total, by = "subj_num") %>%
    left_join(ders_total, by = "subj_num") %>%
    pivot_longer(c(starts_with("patched"), starts_with("adj")),
                 names_to = c("score_type", ".value"),
                 names_pattern = "([a-z]+).(.*)") %>%
    mutate(bpq_total = rowSums(.[grep("bpq", names(.))], na.rm = TRUE),
           tas20_total = rowSums(.[grep("tas20", names(.))], na.rm = TRUE),
           ders_total = rowSums(.[grep("ders", names(.))], na.rm = TRUE))
    
  # write_csv(all, here::here("ignore", "data_gdrive", "emo", data_source, paste0(out_file, ".csv")))
  
  return(all)
}

## senpai online beh wrapper for follow up qs ----

get_senpai_qualtrics_followup <- function(survey_path_numeric, out_file = NULL) {
  
  d_raw <- read_survey(survey_path_numeric) %>%
    # force order as least to most recent,
    # so this removes the LATER responses
    arrange(StartDate) %>%
    rename(subj_num_2 = subj_num) %>%
    rename(subj_num = ResponseId) %>%
    filter(!duplicated(subj_num_2)) %>% 
    mutate(across(where(is.numeric), na_if, -99),
           across(where(is.character), na_if, "-99"))
  
  nas <- get_senpai_qualtrics_followup_nas(d_raw)
  als18_total <- get_senpai_qualtrics_als18(d_raw)
  beq_total <- get_senpai_qualtrics_beq(d_raw)
  cesdr_total <- get_senpai_qualtrics_cesdr(d_raw)
  sticsa_total <- get_senpai_qualtrics_sticsa(d_raw)
  swls_total <- get_senpai_qualtrics_swls(d_raw)
  irq_total <- get_senpai_qualtrics_irq(d_raw)
  # rfq_total <- get_senpai_qualtrics_rfq(d_raw)
  
  all <- nas %>%
    left_join(select(d_raw, subj_num, subj_num_2), by = "subj_num") %>%
    left_join(als18_total, by = "subj_num") %>%
    left_join(beq_total, by = "subj_num") %>%
    left_join(cesdr_total, by = "subj_num") %>%
    left_join(sticsa_total, by = "subj_num") %>%
    left_join(swls_total, by = "subj_num") %>%
    left_join(irq_total, by = "subj_num") %>%
    pivot_longer(c(starts_with("patched"), starts_with("adj")), names_to = c("score_type", ".value"), names_pattern = "([a-z]+).(.*)") %>%
    mutate(als18_total = rowSums(.[grep("als18", names(.))], na.rm = TRUE),
           beq_total = rowMeans(.[grep("beq", names(.))], na.rm = TRUE),
           cesdr_total = rowSums(.[grep("cesdr", names(.))], na.rm = TRUE),
           sticsa_state_total = rowSums(.[grep("sticsa_state", names(.))], na.rm = TRUE),
           sticsa_trait_total = rowSums(.[grep("sticsa_trait", names(.))], na.rm = TRUE),
           irq_total = rowSums(.[grep("irq", names(.))], na.rm = TRUE))
  
  # write_csv(all, here::here("ignore", "data_gdrive", "emo", data_source, paste0(out_file, ".csv")))
  
  return(all)
}
