## setup ----

require(dplyr)
require(tidyr)
require(stringr)
require(magrittr)

## general in-q helpers ----

grab_q_cols <- function (df, pattern) {
  df %>%
    select(subj_num, contains(pattern, ignore.case = F)) %>%
    filter(!is.na(subj_num))
}

pivot_items_legacy <- function (df, q_name = NULL, type, n_likerts = NULL, min_likert = NULL) {
  # q_name only really required if type == "likert"
  # bc it's flexibly thrown out for type == "slider"
  # and n_likerts only required if type == "slider"
  # to transform the scores back into pseudo-likert units of original scale
  if (type == "slider") {
    # As of May 20 2020, it appears that ALL loop/merge (slider) exports from qualtRics
    # come out with the format X_qname_Y where Y is some static filler number counting the number of the slider
    # Y doesn't change because there's only one slider per question
    # X iterates with the loop and merge field
    
    out <- df %>%
      pivot_longer(-subj_num,
                   names_to = c("item", NA, NA),
                   values_to = "response",
                   names_sep = "_",
                   names_transform = list(item = as.integer)) %>% 
      mutate(response = response / (100 / (n_likerts - 1)) + min_likert)
    
  } else if (type == "likert") {
    out <- df %>%
      pivot_longer(-subj_num,
                   names_to = "item",
                   values_to = "response",
                   names_prefix = paste0(q_name, "_"),
                   names_transform = list(item = as.integer))
  }
  
  return (out)
}

summarize_score <- function (df, FUN = sum) {
  df %>%
    summarize(score = FUN(response, na.rm = TRUE),
              score_patched = FUN(response_patched),
              n_items_responded = sum(!is.na(response)),
              n_items_seen = length(response)) %>%
    mutate(score_adj = (score / n_items_responded) * n_items_seen)
}

pivot_subscales <- function (df) {
  df %>%
    select(-score, -n_items_responded, -n_items_seen) %>%
    rename(patched = score_patched, adj = score_adj) %>%
    pivot_wider(names_from = subscale, values_from = c(adj, patched), names_sep = ".")
}

underscore_subscales <- function (df, q_name) {
  df %<>%
    mutate(subscale = tolower(str_replace_all(subscale, "[ -/]", "_")))
  
  if ("q_type" %in% names(df)) {
    df %<>%
      mutate(subscale = paste(q_name, q_type, subscale, sep = "_"))
  } else {
    df %<>%
      mutate(subscale = paste(q_name, subscale, sep = "_"))
  }
}