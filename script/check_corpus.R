# look at ngram counts as predictors of lo

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(broom)
library(lme4)
library(performance)
library(sjPlot)

# -- read -- #

d = read_tsv('dat/mondsz_webcorpus_ngram_counts.tsv')

# -- setup -- #

d = d |> 
  filter(varies) |> 
  mutate(
    phon = case_when(
      bigram1 == 'ng' ~ 'phon_ng',
      bigram1 %in% c('ll','rr','gg', 'nn', 'ṉṉ') ~ 'phon_long',
      bigram2 %in% c('ts','ds','tz','dz') ~ 'phon_c',
      T ~ 'no phon'
    )
  )
  
cols_to_transform = c(
  "trigram_within_all", "trigram_within_verb",
  "bigram1_within_all", "bigram1_within_verb",
  "bigram2_within_all", "bigram2_within_verb"
)

d = d |> 
  mutate(across(
    all_of(cols_to_transform),
    ~ ifelse(. == 0, 0, log(.)),
    .names = "log_{.col}"
  ))

# -- glm -- #

# counts 1
fit1 = glmer(cbind(freq_nv,freq_v) ~ log_trigram_within_all + (1|tag), data = d, family = binomial)
# counts 2
fit2 = glmer(cbind(freq_nv,freq_v) ~ log_bigram2_within_verb + (1|tag), data = d, family = binomial)
# phon
fit3 = glmer(cbind(freq_nv,freq_v) ~ phon + (1|tag), data = d, family = binomial)

compare_performance(fit1,fit2,fit3, metrics = 'common')
test_bf(fit2,fit1)

d$phon_res = resid(fit3)
fit4 = lm(phon_res ~ log_bigram2_within_verb, data = d)
summary(fit4)
# I think it's time to hang my hat
fit5 = lm(phon_res ~ log_trigram_within_all, data = d)
summary(fit5)
# yes, hat, hang.