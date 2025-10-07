# test data too heterogeneous: need to narrow down to -nd or whatever.

################################################
# fit GCM
# is target closer to cc or cvc?
# separately for separate tags
# tune s and p on lemmata to prevent a forking paths explosion
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(broom)

# -- fun -- #

# take specific word, s, p, return word weight
fitGCM = function(dat, var_s, var_p){
  
  my_weight = dat |> 
    mutate(
      pairwise_sim = exp ( - dist / var_s )^var_p,
      total_sim = sum(pairwise_sim)
    ) |> 
    group_by(category) |> 
    mutate(
      category_sim = sum(pairwise_sim),
    ) |> 
    ungroup() |> 
    mutate(
      weight = category_sim / total_sim
    ) |> 
    filter(category == 'vc training') |> 
    distinct(
      weight
    )
  
  return(my_weight)
}

# map fitGCM through an entire word list, return words w/ weights
categoryGCM = function(dat, my_s, my_p){
  dat |>
    nest(.by = test) |> # test, not form
    mutate(
      weight = map(data, ~fitGCM(., var_s = my_s, var_p = my_p))
      ) |>
    select(test, weight) |>
    unnest(
        weight
      )  
}

# take output of categoryGCM, merge with d, return r2 based on deviance (best metric, since glm optimises for it and the glms only differ in what the predictor is, not complexity)
evalGCM = function(d,dat,is_mond = F){
  if (is_mond){
    dat2 = rename(dat, lemma = test)
  } else {
    dat2 = dat
  }
  dat2 = inner_join(d,dat2)
  fit1 = glm(cbind(freq_nv,freq_v) ~ weight, data = dat2, family = binomial)  
  fit0 = glm(cbind(freq_nv,freq_v) ~ 1, data = dat2, family = binomial)
  r2 = 1 - deviance(fit1) / deviance(fit0)
  return(r2)
}

# -- read -- #

t = read_tsv('dat/gcm_distances.gz')
d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- do the basic form (3sg) -- #

t_mond = t |> 
  filter(
    tag_test == 'Prs.NDef.3Sg (mond)'
    )

tuning = crossing(
  var_s = seq(0.01,0.99,0.01),
  var_p = 1:2
) |>
  mutate(
    id = 1:n()
  )

mond_outputs = tuning |> 
  mutate(
    out = map2(var_s, var_p, ~ categoryGCM(t_mond, my_s = .x, my_p = .y)),
    r2 = map_dbl(out, ~ evalGCM(d = d, dat = .x, is_mond = T))
  ) |> 
  arrange(-r2) |> 
  slice(1) |> 
  select(var_s,var_p,r2)
# .83, 1, .41

out_mond = categoryGCM(t_mond, .83, 1) |> 
  rename(lemma = test) |> 
  inner_join(d)

out_mond |> 
  ggplot(aes(lo_v,weight)) +
  geom_point() +
  geom_smooth()
fit1 = glm(cbind(freq_nv,freq_v) ~ weight, data = out_mond, family = binomial)
tidy(fit1)
performance::r2_kullback(fit1)

# this quite obviously breaks somewhere
