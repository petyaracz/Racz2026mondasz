# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)
library(lme4)
library(performance)
library(sjPlot)

# -- read -- #

# distances, info on existing words, results on nonwords
t = read_tsv('distance_maker/aligned_word_pairs_phonological_distance.tsv')
c = read_tsv('dat/mondasz_training.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')

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

coords2 = coords |> 
  left_join(mindists)

d_sum = d |> 
  summarise(
    p_v = mean(resp_v),
    .by = c(coda,tag,lemma_orth)
  ) 

# -- viz: coda and tag -- #

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

d_sum |> 
  filter(coda != 'ng') |> 
  ggplot(aes(p_v, tag, colour = coda)) +
  geom_boxplot() +
  # geom_violin(position = position_dodge(width = 0.9)) +
  # geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
  scale_colour_colorblind() +
  theme_bw()

fit0 = glmer(as.double(resp_v) ~ coda * tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit1 = glmer(as.double(resp_v) ~ coda + tag + (1|raw_id) + (1|lemma), data = d, family = binomial)
plot(compare_performance(fit0,fit1,metrics = 'common'))
plot_model(fit0, 'pred', terms = c("coda","tag"))
plot_model(fit0, 'pred', terms = c("tag","coda"))
d$ng = as.factor(d$coda == 'ng')
fit3 = glmer(as.double(resp_v) ~ ng * tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
plot_model(fit3, 'pred', terms = c("tag","ng"))

# -- viz: coords -- #

# coords |> 
#   ggplot(aes(x,y,label = lemma_orth, fill = type, alpha = lo_v)) +
#   geom_label() +
#   theme_few() +
#   facet_wrap( ~ tag)
# 
# hahaha no

coords |> 
  mutate(lo_v_ntile = ntile(lo_v,4)) |> 
  ggplot(aes(x,y, colour = coda, pch = type)) +
  geom_point() +
  theme_void() +
  facet_wrap( ~ tag + lo_v_ntile) +
  scale_colour_colorblind()

coords2 |> 
  filter(!is.na(group)) |> 
  ggplot(aes(x,y,colour = coda, pch = type, group = as.character(group))) +
  geom_point() +
  geom_line(colour = 'grey') +
  theme_void() +
  facet_wrap( ~ tag) +
  scale_colour_colorblind()

# mds is not very intuitive / reliable apparently
# or word similarity is more weighted by ending and not whole word
sort(unique(c$lemma_orth))
unique(c$coda)
