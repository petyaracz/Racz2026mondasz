# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)
library(patchwork)

# -- read -- #

dists = read_tsv('distance_maker/aligned_word_pairs_phonological_distance.tsv')
training = read_tsv('dat/mondasz_training.tsv')
test = read_tsv('dat/nonwords_first_pass.tsv')

# -- make test long -- #

testl = test |> 
  pivot_longer(-c(lemma,lemma_orth,coda)) |> 
  mutate(
    tag = case_when(
      str_detect(name, 'form_1sg2') ~ "Prs.1Sg›2 (mondalak)",
      str_detect(name, 'form_2sg') ~ "Prs.NDef.2Sg (mondas)",
      str_detect(name, 'form_2pl') ~ "Prs.NDef.2Pl (mondotok)",
      str_detect(name, 'form_3pl') ~  "Prs.NDef.3Pl (mondanak)" 
    ),
    type = case_when(
      str_detect(name, '_1$') ~ 'nv_form',
      str_detect(name, '_2$') ~ 'v_form'
    )
  ) |> 
  select(-name) |> 
  pivot_wider(names_from = type, values_from = value)

# -- distances -- #

# make minimal sets for later

training_min = training |> 
  select(lemma,lemma_orth,tag,lo_v,coda) |> 
  mutate(class = 'training')

test_min = testl |> 
  select(lemma,lemma_orth,tag,coda) |> 
  mutate(class = 'test')

keep_coda = unique(testl$coda)

d_min = training_min |> 
  bind_rows(test_min) |> 
  filter(coda %in% keep_coda)

# create distance matrix

dists_sym = bind_rows(
  dists |> select(word1, word2, phon_dist),
  dists |> select(word1 = word2, word2 = word1, phon_dist)
)

# reshape2's acast outputs a matrix that cmdscale likes

dist_matrix = reshape2::acast(dists_sym, word1 ~ word2, value.var = "phon_dist", fill = 0)

# hammer cmdscale output into something *I* like

my_map = stats::cmdscale(dist_matrix, k = 2) |> 
  as.data.frame() |> 
  rownames_to_column(var = 'lemma') |> 
  rename(x = V1, y = V2)

# now we plot

lemma_id = d_min |> 
  distinct(lemma_orth) |> 
  mutate(id = 1:n())

d_min |> 
  left_join(my_map) |> 
  left_join(lemma_id) |> 
  ggplot(aes(x,y,label = id,colour = lo_v)) +
  geom_text() +
  theme_bw() +
  facet_wrap( ~ tag) +
  scale_colour_viridis_b()
 
d_min |> 
  left_join(my_map) |> 
  left_join(lemma_id) |> 
  ggplot(aes(x,y,label = id, colour = class)) +
  geom_text() +
  theme_bw() +
  facet_wrap( ~ tag) +
  scale_colour_viridis_d()

d_test_dist = d_min |> 
  left_join(my_map) |> 
  left_join(lemma_id) |> 
  filter(class == 'test', tag == "Prs.NDef.2Sg (mondas)") # tag could be anything

d_test_dist |> 
  ggplot(aes(x,y,label = lemma_orth)) +
  geom_text() +
  theme_bw()

kill = dists |> 
  filter(
    word1 %in% d_test_dist$lemma,
    word2 %in% d_test_dist$lemma
         ) |> 
  arrange(phon_dist) |> 
  mutate(coda = str_extract(word1, '..$')) |> 
  group_by(coda) |> 
  distinct(word1) |> 
  slice(1:5) |> 
  pull(word1)

d_test_dist |> 
  filter(!lemma %in% kill) |> 
  ggplot(aes(x,y,label = lemma_orth)) +
  geom_text() +
  theme_bw()# +
  # coord_cartesian(ylim = c(-.5,.25),xlim = c(2,3))

kill2 = d_test_dist |> 
  filter(str_detect(lemma_orth, 'tőrdöszt|sehejt|hartojt|sehejt|numoszt|gyatyojt|szacsojt|sohont|dokont|avont|ubong|höttönt|környönt|gyonong|purnyong|sújlong|kanyvoszt|bázojt')) |> 
  pull(lemma)

kill3 = c(kill,kill2)

d_test_dist |> 
  filter(!lemma %in% kill3) |> 
  count(coda)

keep = d_test_dist |> 
  filter(!lemma %in% kill3) |> 
  group_by(coda) |> 
  sample_n(20)
  
keep |> 
  ggplot(aes(x,y,label = lemma_orth)) +
  geom_text() +
  theme_bw() #+
  coord_cartesian(ylim = c(-.5,0),xlim = c(0,.5))

test |> 
  filter(lemma_orth %in% keep$lemma_orth) |> 
  write_tsv('dat/nonwords_final.tsv')
