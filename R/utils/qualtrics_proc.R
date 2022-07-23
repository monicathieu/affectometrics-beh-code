# Designed to be called BEFORE other scripts to cleanly fresh-load Qualtrics questionnaire data

## @knitr preloading ----
require(tidyverse)
require(magrittr)
require(rlang)

## super DUPER wrapper definition young and old with followup ----

qs_bind <- function (qs_rd1, qs_rd2, qs_rd3) {
  
  # path = here::here("ignore", "data_gdrive", "emo", "online")
  
  qs <- bind_rows(amusement_fear = qs_rd1, positive_negative = qs_rd2, .id = "expt_type") %>%
    # using left_join should ONLY bind the younger adults' follow-ups on
    left_join(qs_rd3 %>%
                select(-subj_num, -nas_total),
              by = c("subj_num" = "subj_num_2", "score_type")) %>%
    filter(score_type == "patched") %>%
    select(-score_type) %>% 
    # younger adults (two rds of task), first round of questionnaires
    mutate(i_panas_sf_neg_z = c(scale(i_panas_sf_neg)),
           i_panas_sf_pos_z = c(scale(i_panas_sf_pos)),
           bpq_total_z = c(scale(bpq_total)),
           tas20_total_z = c(scale(tas20_total)),
           ders_total_z = c(scale(ders_total)),
           gender = coalesce(as.character(gender), "Not reported"),
           gender = factor(gender),
           # second round of questionnaires
           # NAing out Ss who left more than half the qs of a measure blank. freakin followup hoes
           cesdr_total = if_else(nas_cesdr > 10, NA_real_, cesdr_total),
           sticsa_state_total = if_else(nas_sticsa_state > 10, NA_real_, sticsa_state_total),
           sticsa_trait_total = if_else(nas_sticsa_trait > 10, NA_real_, sticsa_trait_total),
           als18_total = if_else(nas_als18 > 9, NA_real_, als18_total),
           beq_total = if_else(nas_beq > 8, NA_real_, beq_total),
           irq_total = if_else(nas_irq > 8, NA_real_, irq_total),
           swls = if_else(nas_swls > 2, NA_real_, swls),
           cesdr_total_z = c(scale(cesdr_total)),
           sticsa_state_total_z = c(scale(sticsa_state_total)),
           sticsa_trait_total_z = c(scale(sticsa_trait_total)),
           als18_total_z = c(scale(als18_total)),
           beq_total_z = c(scale(beq_total)),
           irq_total_z = c(scale(irq_total)),
           swls_z = c(scale(swls)))
  
  # write_csv(qs, paste0(path, "/qualtrics_allsubs.csv"))
  return (qs)
}
