# aggregate results for Results.md

library(tidyverse)

setwd('~/Github/Racz2026mondasz/')

d = read_csv('dat/exp_data_tidy.csv.gz', show_col_types = FALSE)
c = read_tsv('dat/corpus_cc_varies.tsv', show_col_types = FALSE)

# complete cases only
d2 = d[complete.cases(d$resp_v), ]

cat('--- exp V-form choice rate by coda ---\n')
d2 |>
  summarise(p_v = mean(resp_v), lo_v = log(sum(resp_v) / sum(resp_v == FALSE)), n = n(), .by = coda) |>
  arrange(lo_v) |>
  print()

cat('--- exp V-form choice rate by tag ---\n')
d2 |>
  summarise(p_v = mean(resp_v), lo_v = log(sum(resp_v) / sum(resp_v == FALSE)), n = n(), .by = tag) |>
  arrange(lo_v) |>
  print()

# corpus: add consonant cluster variable
c = c |>
  mutate(
    suffix_initial = case_when(
      grepl('2Sg', tag) ~ 's',
      grepl('3Pl', tag) ~ 'n',
      grepl('1Sg', tag) ~ 'l',
      grepl('2Pl', tag) ~ 't'
    ),
    consonants = paste0(coda, suffix_initial)
  )

# experiment: same
d2 = d2 |>
  mutate(
    suffix_initial = case_when(
      grepl('2Sg', tag) ~ 's',
      grepl('3Pl', tag) ~ 'n',
      grepl('1Sg', tag) ~ 'l',
      grepl('2Pl', tag) ~ 't'
    ),
    consonants = paste0(coda, suffix_initial)
  )

cc = c |>
  filter(coda %in% unique(d2$coda)) |>
  summarise(cluster_lo_corpus = log(sum(freq_v) / sum(freq_nv)), .by = consonants)

dc = d2 |>
  summarise(cluster_lo_exp = log(sum(resp_v) / sum(resp_v == FALSE)), .by = consonants)

cors = inner_join(cc, dc, by = 'consonants')

cat('--- per-cluster corpus vs exp log-odds ---\n')
cors |> arrange(cluster_lo_corpus) |> print()

ct = cor.test(cors$cluster_lo_corpus, cors$cluster_lo_exp, method = 'spearman')
cat('Spearman rho:', round(ct$estimate, 3), 'p:', round(ct$p.value, 4), '\n')
