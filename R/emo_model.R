## setup ----

require(dplyr)
require(tidyr)
require(purrr)
require(rlang)
require(magrittr)
source(here::here("R", "utils", "emo_premodel_formulas.R"))
options(mc.cores = parallel::detectCores())

## model wrapper functions ----

fit_emo_model <- function (task, pc_scores, formula, this_emo_block, this_resp_type) {
  # now NOT mapped! to allow the targets to be separated and parallelized via tar_map
  data <- task %>% 
    left_join(pc_scores %>% select(subj_num, PC1, PC2),
              by = "subj_num") %>%
    filter(emo_block == this_emo_block, resp_type == this_resp_type, !is.na(resp))
  
  if (this_resp_type == "cat") {
    
    out <- rstanarm::stan_glmer(formula,
                                family = binomial(link = "logit"),
                                data = data,
                                prior = rstanarm::cauchy(0, 2.5),
                                prior_intercept = rstanarm::cauchy(0, 2.5))
    
  } else if (this_resp_type == "cont") {
    
    out <- data %>% 
      mutate(resp_bounded = case_when(resp == 0 ~ 0.005,
                                      resp == 1 ~ 0.995,
                                      TRUE ~ resp),
             resp_qlogis = qlogis(resp_bounded)) %>% 
      rstanarm::stan_glmer(formula,
                                family = gaussian,
                                data = .,
                                prior = rstanarm::normal(0, 10),
                                prior_intercept = rstanarm::normal(0, 10))
    
  }
  
  return (out)
}
