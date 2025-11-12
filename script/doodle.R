# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(patchwork)

# -- read -- #

c = read_tsv('dat/mondasz_training.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')

# -- doodle -- #

## c

p1 = c |> 
  # filter(coda %in% c('jt','ng','nt','st')) |> 
  mutate(p_v = freq_v/(freq_v+freq_nv)) |> 
  ggplot(aes(p_v,tag,colour = coda)) +
  geom_boxplot() +
  xlab('p(mondas)') +
  ylab('') +
  theme_bw() +
  scale_colour_viridis_d(option = 'F') +
  ggtitle('corpus') +
  theme(legend.position = 'bottom')

## d

my_counts = d |> 
  count(resp_v,lemma_orth,tag,coda) |> 
  pivot_wider(names_from = resp_v, values_from = n, values_fill = 0) |> 
  mutate(p_v = `TRUE` / (`TRUE`+`FALSE`))

my_counts |> 
  ggplot(aes(tag,p_v)) +
  geom_boxplot()

my_counts |> 
  ggplot(aes(p_v,coda,colour = tag)) +
  geom_boxplot()

p2 = my_counts |> 
  ggplot(aes(p_v,tag,colour = coda)) +
  geom_boxplot() +
  xlab('p(mondas)') +
  ylab('') +
  theme_bw() +
  scale_colour_viridis_d(option = 'H') +
  ggtitle('experiment') +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = 'bottom')

p1 + p2
ggsave('viz/res.png', dpi = 'print', width = 10, height = 6)
