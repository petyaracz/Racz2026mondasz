################################################
# fit SVM
# is target closer to cc or cvc?
# separately for separate tags
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)

# -- fun -- #

fitGCM = function(dat, var_s, var_p){
  
  dists = dat |> 
    mutate(
      pairwise_sim = exp ( - dist / var_s )^var_p,
      total_sim = sum(pairwise_sim)
    ) |> 
    group_by(class) |> 
    mutate(
      category_sim = sum(pairwise_sim),
    ) |> 
    ungroup() |> 
    mutate(
      weight = category_sim / total_sim
    ) |> 
    filter(class == 'vc') |> 
    distinct(
      test,weight
    )
  
  return(dists)
}

# -- read -- #

t = read_tsv('dat/distances.gz')
d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- wrangle -- #

fitGCMspec = partial(fitGCM, var_s = .01, var_p = 2)

t_nested = t |> 
  nest(.by = c(lemma_orth,tag_test))

fits = t_nested |> 
  mutate(
    weight = map(data, fitGCMspec)
  )

d2 = fits |> 
  select(lemma_orth,tag_test,weight) |> 
  rename(tag = tag_test) |> 
  unnest(weight) |> 
  left_join(d)

d2 |> 
  ggplot(aes(lo_v,weight)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ tag)

# need to incorporate lemma-level info as well