## setup ----

require(dplyr)
require(magrittr)
require(rlang)

## load data ----

get_pca_qs <- function (qs, scrubs) {
  goodsubs <- scrubs %>% 
    filter(enough_trials_seen, enough_resps, not_too_fast, slider_not_lazy, age_valid)
  
  qs <- qs %>%
    filter(subj_num %in% unique(goodsubs$subj_num))
  
  q_names <- c("i_panas_sf_pos",
               "i_panas_sf_neg",
               "bpq_total",
               "tas20_total",
               "ders_total",
               "cesdr_total",
               "sticsa_state_total",
               "sticsa_trait_total",
               "als18_total",
               "beq_total",
               "irq_total",
               "swls")
  
  # Only Ss who a) survive beh scrubbing b) survive follow-up scrubbing
  qs_complete <- qs %>%
    group_by(subj_num, expt_type) %>%
    select(!!!syms(c(q_names))) %>%
    distinct() %>%
    ungroup() %>%
    filter(complete.cases(.))
  
  return (qs_complete)
}

## estimate qs PCA ----

estimate_pca <- function (qs) {
  
  pca <- qs %>%
    select(-subj_num, -expt_type) %>%
    prcomp(center = TRUE, scale. = TRUE)
  
  return (pca)
  
}

get_pc_scores <- function (pca, qs) {
  # CENTERING/SCALING PCs for greater interpretability in subsequent regressions
  scores <- pca$x %>%
    scale() %>%
    as_tibble() %>%
    mutate(subj_num = qs$subj_num,
           expt_type = qs$expt_type)
  
  return (scores)
}
## save out ----

# write_csv(qs_youngs_complete_pcs, here::here("ignore", "data_gdrive", "emo", "online", "pcs_goodsubs.csv"))

# save(qs_pca, file = here::here("ignore", "data_gdrive", "emo", "online", "pcs.rda"))
