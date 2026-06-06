# explore corpus and experiment data for mondasz/mondsz variation
# compares corpus log-odds with forced-choice experiment preferences
# across four inflectional slots and four coda types
# script logic follows things reported in paper

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)  # fancy plots
library(knitr)     # kable tables
library(patchwork) # combining ggplots side by side
library(googlesheets4) # to shovel tables into the gdoc

# -- read -- #

c  = read_tsv('dat/corpus_cc_varies.tsv')  # corpus data: only lemmas with attested variation
# c2 = read_tsv('dat/corpus_cc.tsv')         # corpus data: all CC-final verbs from Webcorpus 2
d  = read_csv('dat/exp_data_tidy.csv.gz')  # forced-choice experiment responses

# -- sums -- #

d |> 
  distinct(raw_id,gender) |> 
  count(gender)

d |> 
  summarise(median(yob),median(total_time))

# -- filter d -- #

d = d |> 
  filter(checks_passed %in% c('8 / 8', '7 / 8', '6 / 8')) # 75/76

d = d |> 
  filter(rt < rt_upper) # 917/24320

# -- transform -- #

# compute suffix-initial consonant and full NV consonant cluster (coda + suffix onset)
# for both corpus and experiment data

c = c |>
  mutate(
    coda2 = ifelse(coda %in% d$coda, str_replace_all(coda, 'st','szt'), NA),
    tag = str_replace(tag, 'mondas', 'mondasz'),
    suffix_initial = case_when(
      str_detect(tag, '2Sg') ~ 's',  # -sz / -asz: suffix onset is s
      str_detect(tag, '3Pl') ~ 'n',  # -nak / -nek: suffix onset is n
      str_detect(tag, '1Sg') ~ 'l',  # -lak / -lek: suffix onset is l
      str_detect(tag, '2Pl') ~ 't'   # -tok / -tek: suffix onset is t
    ),
    consonants = paste0(coda, suffix_initial),
    consonants2 = str_replace_all(consonants, 's', 'sz')
  )

d = d |>
  mutate(
    coda2 = coda |> str_replace('st','szt'),
    suffix_initial = case_when(
      str_detect(tag, '2Sg') ~ 's',
      str_detect(tag, '3Pl') ~ 'n',
      str_detect(tag, '1Sg') ~ 'l',
      str_detect(tag, '2Pl') ~ 't'
    ),
    consonants = paste0(coda, suffix_initial),
    consonants2 = str_replace_all(consonants, 's', 'sz')
  )

# per-item summary for experiment: log(n_v / n_nv) per lemma x coda x tag
# resp_v is boolean; sum(resp_v) = V-form choices, sum(!resp_v) = NV-form choices
d_item = d |>
  summarise(
    lo_v = log(sum(resp_v) / sum(!resp_v)),
    .by = c(lemma_orth, coda, tag)
  )

# per-item summary collapsed across tags: log(n_v / n_nv) per lemma x consonant cluster
d_item_cluster = d |>
  summarise(
    lo_v = log(sum(resp_v) / sum(!resp_v)),
    .by = c(lemma_orth, consonants, consonants2)
  )

# factor level order for consonant clusters: ranked by corpus log-odds
# aggregate corpus freq_v and freq_nv per cluster, then compute log-odds
# so both corpus and experiment plots share the same y-axis ordering
consonants_fct = c |>
  filter(coda %in% unique(d$coda)) |>
  summarise(lo_v = log(sum(freq_v) / sum(freq_nv)), .by = consonants) |>
  mutate(consonants = fct_reorder(consonants, lo_v)) |>
  pull(consonants)

consonants2_fct = c |>
  filter(coda %in% unique(d$coda)) |>
  summarise(lo_v = log(sum(freq_v) / sum(freq_nv)), .by = consonants2) |>
  mutate(consonants2 = fct_reorder(consonants2, lo_v)) |>
  pull(consonants2)

# apply shared factor order to corpus and experiment cluster data
cb = c |>
  filter(coda %in% unique(d$coda)) |>
  mutate(
    consonants = factor(consonants, levels = levels(consonants_fct)),
    consonants2 = factor(consonants2, levels = levels(consonants2_fct))
         )

db = d_item_cluster |>
  mutate(
    consonants = factor(consonants, levels = levels(consonants_fct)),
    consonants2 = factor(consonants2, levels = levels(consonants2_fct))
    )

# cluster-level log-odds for corpus and experiment (used in correlation and scatter)
# corpus: log(sum(freq_v) / sum(freq_nv)) per cluster
# experiment: log(sum(resp_v) / sum(!resp_v)) per cluster, aggregated from trial-level data
cc = cb |>
  summarise(cluster_lo = log(sum(freq_v) / sum(freq_nv)), .by = consonants2) |>
  mutate(type = 'corpus')

dc = d |>
  filter(consonants2 %in% levels(consonants2_fct)) |>
  mutate(consonants2 = factor(consonants2, levels = levels(consonants2_fct))) |>
  summarise(cluster_lo = log(sum(resp_v) / sum(!resp_v)), .by = consonants2) |>
  mutate(type = 'exp')

# wide table: one row per consonant cluster, corpus and exp log-odds side by side
cors = bind_rows(dc, cc) |>
  pivot_wider(names_from = type, values_from = cluster_lo)

# -- tables -- #

# all codas in c
t1 = c |> 
  mutate(
    n = n(),
    .by = coda
           ) |> 
  arrange(-llfpm10) |>
  group_by(coda,n) |>
  slice(1:3) |>
  mutate(a_per_b = paste(form_v_orth, form_nv_orth, sep = '/')) |>
  summarise(corpus = paste(a_per_b, collapse = ', '))

DescTools::Gini(t1$n)

t1
# t1 |> 
#   write_sheet('https://docs.google.com/spreadsheets/d/1HAQA_DwaGbVw1amDyCz8p78L8wMZTEAcdpmK_liKBxU/edit?usp=sharing', 't1')

t2 = c |>
  filter(
    lemma_orth %in% c('rajong','dühöng','csüng'),
    coda == 'ng', 
    xpostag == '[/V][Prs.NDef.2Sg]'
         ) |>
  arrange(lo_v) |> 
  select(lemma_orth,form_v_orth,form_nv_orth,freq_v,freq_nv,odds_v,lo_v)

# t2 |> 
#   mutate_if(is.double, ~ round(., 2)) |> 
#   write_sheet('https://docs.google.com/spreadsheets/d/1HAQA_DwaGbVw1amDyCz8p78L8wMZTEAcdpmK_liKBxU/edit?usp=sharing', 't2')

# top 3 corpus examples per tag x coda combination, ranked by lemma frequency
c |>
  filter(coda %in% unique(d$coda)) |>
  arrange(-llfpm10) |>
  group_by(tag, coda) |>
  slice(1:3) |>
  mutate(a_per_b = paste(form_v_orth, form_nv_orth, sep = '/')) |>
  summarise(corpus = paste(a_per_b, collapse = ', ')) |>
  kable('simple')

# 3 random experiment nonwords per tag x coda combination
t3 = d |>
  group_by(tag, coda) |>
  sample_n(3) |>
  mutate(a_per_b = paste(v_form, nv_form, sep = '/')) |>
  summarise(exp = paste(a_per_b, collapse = ', '))

# t3 |> 
#   write_sheet('https://docs.google.com/spreadsheets/d/1HAQA_DwaGbVw1amDyCz8p78L8wMZTEAcdpmK_liKBxU/edit?usp=sharing', 't3')


# -- figures -- #

# 1. corpus: all coda types, excluding very rare clusters
c |>
  filter(!coda %in% c('dz', 'mt', 'nl', 'št', 'ts')) |>
  ggplot(aes(tag, lo_v)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ylab('log(kötőhangzó/nincs kötőhangzó)') +
  facet_wrap(~ coda) +
  ggtitle('tővégi mássalhangzócsoportok a webkorpuszban')

# not looking good.

# 2. corpus: restricted to the four coda types used in the experiment
c |>
  filter(!is.na(coda2)) |>
  ggplot(aes(tag, lo_v)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.3, size = 1, color = "grey40") +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~ coda2) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.01,.5,.99), name = 'p(kötőhangzó)'), limits = c(-7,10), name = 'log(kötőhangzó/nincs kötőhangzó)', breaks = c(-5,-2,0,2,5)) +
  ggtitle('tővégi mássalhangzócsoportok\na webkorpuszban')

ggsave('viz/fig1.png', dpi = 'print', width = 5, height = 5)

# 3. experiment: per-item log(kötőhangzó/nincs kötőhangzó) by tag, faceted by coda
d_item |>
  ggplot(aes(tag, lo_v)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.3, size = 1, color = "grey40") +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ylab('log(kötőhangzó/nincs kötőhangzó)') +
  facet_wrap(~ coda) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.5,.75,.9), name = 'p(kötőhangzó)'), limits = c(-1.5,2.5), name = 'log(kötőhangzó/nincs kötőhangzó)', breaks = c(-1,0,1,2)) +
  ggtitle('tővégi mássalhangzócsoportok\na kísérletben')

ggsave('viz/fig2.png', dpi = 'print', width = 5, height = 5)

# 4. experiment: per-item log(kötőhangzó/nincs kötőhangzó) by coda, faceted by tag
d_item |>
  ggplot(aes(coda, lo_v)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  ylab('log(kötőhangzó/nincs kötőhangzó)') +
  facet_wrap(~ tag) +
  ggtitle('experiment: by coda and tag')

# 5. corpus and experiment side by side, both ordered by corpus log-odds per cluster
p1 = cb |>
  ggplot(aes(consonants2, lo_v)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.3, size = 1, color = "grey40") +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  coord_flip() +
  xlab('tővégi mássalhangzók +\ntoldalékkezdő mássalhangzó') +
  ylab('log(kötőhangzó/nincs kötőhangzó)') +
  ggtitle('webkorpusz') + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.01,.5,.99), name = 'p(kötőhangzó)'), limits = c(-7,10), name = 'log(kötőhangzó/nincs kötőhangzó)', breaks = c(-5,-2,0,2,5)) 

p2 = db |>
  ggplot(aes(consonants2, lo_v)) +
  geom_jitter(width = 0.25, height = 0, alpha = 0.3, size = 1, color = "grey40") +
  geom_tufteboxplot(median.type = "line", hoffset = 0, width = 3) +
  theme_bw() +
  coord_flip() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab('log(kötőhangzó/nincs kötőhangzó)') +
  ggtitle('kísérlet') + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~ plogis(.), breaks = c(.5,.75,.9), name = 'p(kötőhangzó)'), limits = c(-1.5,2.5), name = 'log(kötőhangzó/nincs kötőhangzó)', breaks = c(-1,0,1,2))

p1 + p2 + plot_layout(axes = 'collect')

ggsave('viz/fig3.png', dpi = 'print', width = 6, height = 6)

# 6. scatter: corpus log(freq_v/freq_nv) vs experiment log(n_v/n_nv) per consonant cluster
cors |>
  ggplot(aes(exp, corpus, label = consonants2)) +
  geom_text() +
  theme_bw() +
  xlab('kísérlet log(kötőhangzó/nincs kötőhangzó)') +
  ylab('webkorpusz log(kötőhangzó/nincs kötőhangzó)') +
  ggtitle('tővégi mássalhangzók +\ntoldalékkezdő mássalhangzó arányok')

ggsave('viz/fig4.png', dpi = 'print', width = 4, height = 4)

# -- analysis -- #

# Spearman rank correlation between corpus and experiment log-odds per cluster
# tests whether clusters favouring the V-form in the corpus also do so in the experiment
cor.test(cors$corpus, cors$exp, method = 'spearman')
