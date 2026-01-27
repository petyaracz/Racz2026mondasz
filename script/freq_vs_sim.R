d = read_tsv('dat/krr_results_experiment_freq.tsv')
d2 = read_tsv('dat/krr_results_experiment.tsv')

library(lme4)

pred1 = d2 |> 
  rename(krr_pred_lov = krr_pred) |> 
  distinct(lemma,lemma_orth,krr_pred_lov)

d2 = d |> 
  rename(krr_pred_freq = krr_pred) |> 
  left_join(pred1)

d3 = d2 |> 
  count(
    resp_v, lemma, lemma_orth, krr_pred_lov, krr_pred_freq, coda, tag
  ) |> 
  pivot_wider(names_from = resp_v, values_from = n) |> 
  rename(resp_v = `TRUE`, resp_nv = `FALSE`)

lm1 = glm(cbind(resp_v,resp_nv) ~ coda + tag + krr_pred_lov, data = d3, family = binomial)

summary(lm1)

lm2 = glm(cbind(resp_v,resp_nv) ~ coda + tag + krr_pred_freq, data = d3, family = binomial)

summary(lm2)
# :(