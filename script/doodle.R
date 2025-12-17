# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)
library(lme4)
library(performance)
library(sjPlot)
library(patchwork)

# -- read -- #

# distances, info on existing words, results on nonwords
t = read_tsv('distance_maker/aligned_word_pairs_phonological_distance.tsv')
c = read_tsv('dat/mondasz_training.tsv')
c2 = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')
p = read_tsv('distance_maker/siptar_torkenczy_toth_racz_hungarian_st_julia.tsv')

# -- devices -- #

d2 = d |> 
  mutate(
    user_info2 = str_extract(user_info, '(?<=^device: )[^,]*(?=,)')
    )

d2 |> 
  distinct(raw_id,user_info2) |>
  count(user_info2) |> 
  mutate(user_info2 = fct_reorder(user_info2, n)) |> 
  ggplot(aes(user_info2,n)) +
  geom_col()

# -- coda -- #

getCodaSim = function(coda1,coda2){
  
  c11 = str_extract(coda1, '^.')
  c12 = str_extract(coda1, '.$')
  c21 = str_extract(coda2, '^.')
  c22 = str_extract(coda2, '.$')
  
  s1 = p[p$segment1 == c11 & p$segment2 == c21,]$similarity
  s2 = p[p$segment1 == c12 & p$segment2 == c22,]$similarity
  
  sim = mean(s1,s2)
  dist = 1-sim
  return(dist)
}

codas = crossing(
  coda1 = unique(c$coda),
  coda2 = unique(c$coda)
) |> 
  mutate(
    coda_dist = map2_dbl(coda1,coda2, ~ getCodaSim(.x,.y))
  )

# -- dist -- #

# c = c |> filter(lo_v > -4, lo_v < 4)

## coords: word coordinates and behaviour

dist = t |> 
  filter(
    word1 %in% c(unique(c$lemma),unique(d$lemma)),
    word2 %in% c(unique(c$lemma),unique(d$lemma))
         ) |> 
  select(word1,word2,phon_dist)

dist2 = tibble(
  word1 = dist$word2,
  word2 = dist$word1,
  phon_dist = dist$phon_dist
)

dist = bind_rows(dist,dist2)

dist2 = dist |> 
  pivot_wider(names_from = word2, values_from = phon_dist)

dist_matrix = dist2 |> 
  select(-word1) |> 
  as.matrix()

dist_matrix[is.na(dist_matrix)] = 0

dist_coord = stats::cmdscale(dist_matrix, k = 2) |> 
  as.data.frame() |> 
  rename(x = V1, y = V2)

dist3 = dist2 |> 
  select(word1) |> 
  rename(lemma = word1) |> 
  bind_cols(dist_coord)

c_keep = c |>
  select(lemma,lemma_orth,tag,coda,lo_v) |> 
  mutate(type = 'real')

d_keep = d |> 
  summarise(
    lo_v = qlogis(mean(resp_v)),
    .by = c(lemma,lemma_orth,tag,coda)
  ) |> 
  mutate(type = 'nonword')

keep = bind_rows(c_keep,d_keep)

coords = left_join(dist3,keep)

## dists: for each nonword, nearest real word and its behaviour 

mindist = dist |> 
  filter(
    word1 %in% d_keep$lemma,
    word2 %in% c_keep$lemma
         ) |> 
  group_by(word1) |> 
  filter(phon_dist == min(phon_dist))

mindists = mindist |> 
  ungroup() |> 
  mutate(group = 1:n()) |> 
  pivot_longer(-c(group,phon_dist)) |> 
  rename(
    min_dist = phon_dist,
    lemma = value
         ) |> 
  select(group,min_dist,lemma)

# will yell at you:
coords2 = coords |> 
  left_join(mindists)

# -- sonority or whatever -- #

d = d |> 
  mutate(
    suffix_c = case_when(
      tag == 'Prs.NDef.2Sg (mondas)' ~ 's',
      tag == 'Prs.NDef.3Pl (mondanak)' ~ 'n',
      tag == 'Prs.1Sg›2 (mondalak)' ~ 'l',
      tag == 'Prs.NDef.2Pl (mondotok)' ~ 't'
    ),
    sequence = glue::glue('{coda}(v){suffix_c}')
  )

d_sum = d |> 
  summarise(
    p_v = mean(resp_v),
    .by = c(coda,tag,lemma_orth,sequence)
  ) |> 
  mutate(sequence = fct_reorder(sequence, p_v))

c = c |> 
  mutate(
    suffix_c = case_when(
      tag == 'Prs.NDef.2Sg (mondas)' ~ 's',
      tag == 'Prs.NDef.3Pl (mondanak)' ~ 'n',
      tag == 'Prs.1Sg›2 (mondalak)' ~ 'l',
      tag == 'Prs.NDef.2Pl (mondotok)' ~ 't'
    ),
    sequence = glue::glue('{coda}(v){suffix_c}')
  )

c2 = c2 |> 
  mutate(
    suffix_c = case_when(
      tag == 'Prs.NDef.2Sg (mondas)' ~ 's',
      tag == 'Prs.NDef.3Pl (mondanak)' ~ 'n',
      tag == 'Prs.1Sg›2 (mondalak)' ~ 'l',
      tag == 'Prs.NDef.2Pl (mondotok)' ~ 't'
    ),
    sequence = glue::glue('{coda}(v){suffix_c}')
  )

c2_sum = c2 |> 
  filter(
    !varies,
    sequence %in% d_sum$sequence
    )

# -- viz: coda and tag -- #

p1 = d_sum |> 
  ggplot(aes(p_v, sequence)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle('experiment')

p2 = c |> 
  mutate(
    sequence = factor(sequence, levels = levels(d_sum$sequence)),
    p_v = plogis(lo_v)
    ) |> 
  filter(!is.na(sequence)) |> 
  ggplot(aes(p_v, sequence)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ggtitle('corpus')

p1 + p2

d_sum |> 
  ggplot(aes(p_v, coda, colour = tag)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  scale_colour_colorblind() +
  theme_bw()

d_sum |> 
  ggplot(aes(p_v, tag, colour = coda)) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  scale_colour_colorblind() +
  theme_bw()

fit0 = glmer(as.double(resp_v) ~ coda * tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit1 = glmer(as.double(resp_v) ~ coda + tag + (1|raw_id) + (1|lemma), data = d, family = binomial)
plot(compare_performance(fit0,fit1,metrics = 'common'))
plot_model(fit0, 'pred', terms = c("coda","tag"))
plot_model(fit0, 'pred', terms = c("tag","coda"))

# -- viz: coords -- #

# coords |> 
#   ggplot(aes(x,y,label = lemma_orth, fill = type, alpha = lo_v)) +
#   geom_label() +
#   theme_few() +
#   facet_wrap( ~ tag)
# 
# hahaha no

coords |> 
  filter(!is.na(lo_v)) |> 
  mutate(lo_v_ntile = ntile(lo_v,4)) |> 
  ggplot(aes(x,y, colour = type)) +
  geom_point() +
  theme_void() +
  facet_wrap( ~ tag + lo_v_ntile) +
  scale_colour_colorblind()

coords |> 
  filter(!is.na(lo_v)) |> 
  ggplot(aes(x,y, colour = type, alpha = lo_v)) +
  geom_point() +
  theme_void() +
  facet_wrap( ~ tag) +
  scale_colour_colorblind()
