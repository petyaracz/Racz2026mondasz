# check corpus counts for stem-final clusters and see how well that predicts participant behaviour

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(lme4)
library(performance)
library(sjPlot)
library(glue)
library(duckdb)

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's', 'x' = 'ks'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
  }
}

# count types for lemmas / forms in webcorpus

countMatches = function(c2, my_ngram, my_type, my_pos){
  if (my_type == 'lemma' & my_pos == 'noun'){
    c2 |> 
      filter(pos == 'noun') |> 
      distinct(lemma_ipa) |> 
      mutate(pass = str_detect(lemma_ipa,my_ngram)) |> 
      filter(pass) |> 
      nrow()
  } else if (my_type == 'lemma' & my_pos == 'verb'){
    c2 |> 
      filter(pos == 'verb') |> 
      distinct(lemma_ipa) |> 
      mutate(pass = str_detect(lemma_ipa,my_ngram)) |> 
      filter(pass) |> 
      nrow()
  } else if (my_type == 'form' & my_pos == 'noun'){
    c2 |> 
      filter(pos == 'noun') |> 
      distinct(form_ipa) |> 
      mutate(pass = str_detect(form_ipa,my_ngram)) |> 
      filter(pass) |> 
      nrow()
  } else if (my_type == 'form' & my_pos == 'verb'){
    c2 |> 
      filter(pos == 'verb') |> 
      distinct(form_ipa) |> 
      mutate(pass = str_detect(form_ipa,my_ngram)) |> 
      filter(pass) |> 
      nrow()
  }
}

# -- read -- #

d = read_tsv('dat/exp_data_tidy.tsv.gz')
c = read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet')

# -- filter -- #

# tidy up c
c2 = c |> 
  filter(hunspell, freq > 10) |> 
  group_by(lemma, form) |>
  slice_max(freq, n = 1, with_ties = FALSE) |>
  ungroup()

# free up space
rm(c);gc()

# -- count -- #
  
# define ngrams
d = d |> 
  mutate(
    c = case_when(
      str_detect(tag, 'mondas') ~ 's',
      str_detect(tag, 'mondanak') ~ 'n',
      str_detect(tag, 'mondalak') ~ 'l',
      str_detect(tag, 'mondotok') ~ 't'
    ),
    trigram = glue('{coda}{c}'),
    bigram1 = coda,
    bigram2 = str_extract(trigram, '..$')
  )

# extract ngrams
ngrams = d |> 
  distinct(trigram,bigram1,bigram2) |> 
  mutate(ngram_id = 1:n()) |> 
  pivot_longer(-ngram_id, names_to = 'type', values_to = 'ngram')

# filter c
c2 = c2 |> 
  mutate(
    lemma_ipa = transcribeIPA(lemma, 'single'),
    form_ipa = transcribeIPA(form, 'single'),
    pos = case_when(
      str_detect(xpostag, '^\\[\\/N\\]') ~ 'noun',
      str_detect(xpostag, '^\\[\\/V\\]') ~ 'verb'
    )
  )

# get argument combinations for counts
ngram_counts = crossing(
  ngrams,
  my_pos = c('verb','noun'),
  my_type = c('lemma','form')
)

ngram_counts = ngram_counts |>
  mutate(
    count = pmap_dbl(
      list(ngram, my_type, my_pos),
      \(my_ngram, my_type, my_pos) countMatches(c2, my_ngram, my_type, my_pos)
    )
  )

ngram_counts = ngram_counts |> 
  pivot_wider(names_from = my_type, values_from = count) |> 
  mutate(form = form - lemma) |> 
  pivot_longer(-c(ngram_id,type,ngram,my_pos), names_to = 'my_type', values_to = 'count')

ngram_counts = ngram_counts |> 
  mutate(
    log_count = scale(log(count)),
    .by = c(type,my_pos,my_type)
  )

# -- format -- #

# 1. Build a lookup table keyed by BOTH ngram AND type
ngram_lookup = ngram_counts |>
  mutate(col_name = paste(my_pos, my_type, "log_count", sep = "_")) |>
  select(type, ngram, col_name, log_count) |> 
  distinct() |> 
  pivot_wider(names_from = col_name, values_from = log_count)

# 2. Join once per ngram type, matching on both ngram value and type
ngram_types = c("bigram1", "bigram2", "trigram")

result = d
for (ng_type in ngram_types) {
  lookup_renamed = ngram_lookup |>
    filter(type == ng_type) |>
    select(-type) |>
    rename_with(~ paste0(ng_type, "_", .x), -ngram)
  
  result = result |>
    left_join(lookup_renamed, by = setNames("ngram", ng_type))
}

# -- find best ngram -- #

result$resp_v = as.double(result$resp_v)

fit_bigram1_noun_lemma_log_count = glmer(resp_v ~ bigram1_noun_lemma_log_count + tag + (1 + bigram1_noun_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram1_verb_lemma_log_count = glmer(resp_v ~ bigram1_verb_lemma_log_count + tag + (1 + bigram1_verb_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram1_noun_form_log_count = glmer(resp_v ~ bigram1_noun_form_log_count + tag + (1 + bigram1_noun_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram1_verb_form_log_count = glmer(resp_v ~ bigram1_verb_form_log_count + tag + (1 + bigram1_verb_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)

fit_bigram2_noun_lemma_log_count = glmer(resp_v ~ bigram2_noun_lemma_log_count + tag + (1 + bigram2_noun_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram2_verb_lemma_log_count = glmer(resp_v ~ bigram2_verb_lemma_log_count + tag + (1 + bigram2_verb_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram2_noun_form_log_count = glmer(resp_v ~ bigram2_noun_form_log_count + tag + (1 + bigram2_noun_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_bigram2_verb_form_log_count = glmer(resp_v ~ bigram2_verb_form_log_count + tag + (1 + bigram2_verb_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)

fit_trigram_noun_lemma_log_count = glmer(resp_v ~ trigram_noun_lemma_log_count + tag + (1 + trigram_noun_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_trigram_verb_lemma_log_count = glmer(resp_v ~ trigram_verb_lemma_log_count + tag + (1 + trigram_verb_lemma_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_trigram_noun_form_log_count = glmer(resp_v ~ trigram_noun_form_log_count + tag + (1 + trigram_noun_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit_trigram_verb_form_log_count = glmer(resp_v ~ trigram_verb_form_log_count + tag + (1 + trigram_verb_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)

ngram_comparisons = compare_performance(
  fit_bigram1_noun_lemma_log_count,
  fit_bigram1_verb_lemma_log_count,
  fit_bigram1_noun_form_log_count,
  fit_bigram1_verb_form_log_count,
  fit_bigram2_noun_lemma_log_count,
  fit_bigram2_verb_lemma_log_count,
  fit_bigram2_noun_form_log_count,
  fit_bigram2_verb_form_log_count,
  # fit_trigram_noun_lemma_log_count,
  # fit_trigram_verb_lemma_log_count,
  # fit_trigram_noun_form_log_count,
  # fit_trigram_verb_form_log_count,
  metrics = 'common'
)

ngram_comparisons |> 
  arrange(AIC)
ngram_comparisons |> 
  arrange(BIC)

plot_model(fit_bigram2_verb_form_log_count, 'est')
plot_model(fit_bigram2_verb_form_log_count, 'pred', terms = c('bigram2_verb_form_log_count','tag'))
check_collinearity(fit_bigram2_verb_form_log_count)
summary(fit_bigram2_verb_form_log_count)
# the positive estimate for bigram2 is completely baffling

fit2 = glmer(resp_v ~ bigram2_verb_form_log_count + (1 + bigram2_verb_form_log_count|raw_id) + (1|lemma), data = result, family = binomial)
fit3 = glmer(resp_v ~ tag + (1 + tag|raw_id) + (1|lemma), data = result, family = binomial)

plot_model(fit2, 'pred', terms = 'bigram2_verb_form_log_count')
plot_model(fit3, 'pred', terms = 'tag')

fit4 = glmer(resp_v ~ coda + tag + (1 + tag|raw_id) + (1|lemma), data = result, family = binomial)
fit5 = glmer(resp_v ~ coda * tag + (1 + tag|raw_id) + (1|lemma), data = result, family = binomial)
compare_performance(fit4,fit5)

summary(fit5)
check_collinearity(fit5)
# time to let this one go