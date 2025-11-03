# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)

# -- read -- #

d = read_tsv('dat/exp_data_tidy.tsv')

# -- doodle -- #

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
