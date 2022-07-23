formula_cat <- resp ~ stim_level + PC1 + PC2 +
  stim_level:PC1 + stim_level:PC2 +
  rt_quantile_75 + n_trials_responded + (1 + stim_level | subj_num)

formula_cont <- resp_qlogis ~ stim_level + PC1 + PC2 +
  stim_level:PC1 + stim_level:PC2 +
  n_trials_responded + (1 + stim_level | subj_num)
