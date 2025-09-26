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

fitGCM = function(dat, var_s, var_p){
  
  dists = dat |> 
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
      test,weight
    )
  
  return(dists)
}

# maybe a second fun that maps this through

categoryGCM = function(dat, my_s, my_p){
  dat |>
  nest(.by = form) |>
  mutate(
    weight = map(data, ~fitGCM(., var_s = my_s, var_p = my_p))
    ) |>
  select(form, weight) |>
  unnest(
    weight
    )  
}

# and an eval function!

# ...

# -- read -- #

t = read_tsv('dat/distances.gz')
d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- wrangle -- #

l = t |>
  filter(tag == '...')

tuning = crossing(
  var_s = seq(0.01,0.99,0.01),
  var_p = 1:2
) |>
mutate(
  id = 1:n()
  )



fits = t_nested |> 
  mutate(
    weight = map(data, fitGCMspec)
  ) |> 
  rename(tag = tag_test) |> 
  select(lemma_orth,tag,weight) |>
  unnest(weight)

d2a = d |> 
  filter(!is.na(freq_v),!is.na(freq_nv)) |> 
  summarise(
    freq_v = sum(freq_v),
    freq_nv = sum(freq_nv),
    .by = lemma_orth
  ) |> 
  mutate(
    lo_v_lemma = log(freq_v/freq_nv)
  ) |> 
  select(lemma_orth,lo_v_lemma)

d2b = fits |> 
  filter(tag == 'Prs.NDef.3Sg (mond)') |> 
  rename(weight_lemma = weight) |> 
  select(lemma_orth,weight_lemma) |> 
  left_join(d2a)

d2c = inner_join(fits,d)

d3 = left_join(d2b,d2c)

d4 = d3 |> 
  select(freq_v,freq_nv,lemma_orth,lo_v,tag,weight,weight_lemma) |> 
  pivot_longer(-c(freq_v,freq_nv,lemma_orth,lo_v,tag), names_to = 'weight_type', values_to = 'weight') |> 
  add_count(lemma_orth) |> 
  mutate(
    scaled_weight = scale(weight),
    lemma_2 = ifelse(n > 10, lemma_orth, 'other')
  )

d4 |> 
  ggplot(aes(lo_v,weight, colour = weight_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ tag) +
  scale_colour_grey() +
  theme_bw()
