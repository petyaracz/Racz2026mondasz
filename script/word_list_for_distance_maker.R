setwd('~/Github/Racz2026mondasz/')

library(tidyverse)

c = read_tsv('dat/mondasz_training.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')

my_vec = bind_rows(distinct(c,lemma),distinct(d,lemma))

write_tsv(my_vec, 'dat/word_list_for_distance_maker.tsv')

241*241
