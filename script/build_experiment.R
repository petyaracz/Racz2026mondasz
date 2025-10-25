# -- head -- #

library(tidyverse)
library(glue)
library(jsonlite)

setwd('~/Github/Racz2026mondasz/')

# -- read -- #

d = read_tsv('dat/nonwords_final.tsv')

# -- setup -- #

# one list for everyone, easy peasy

# make d long

dl = d |> 
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
  select(-name,-coda,-lemma) |> 
  pivot_wider(names_from = type, values_from = value)

# add exp-relevant rows

exp = dl |> 
  rowwise() |> 
  mutate(
    prompt = case_when(
      tag == 'Prs.1Sg›2 (mondalak)' ~ glue('Mari szívesen {lemma_orth}. Téged én is szívesen...,'),
      tag == 'Prs.NDef.2Pl (mondotok)' ~ glue('Mari szívesen {lemma_orth}. Ti is szívesen...,'),  
      tag == 'Prs.NDef.3Pl (mondanak)' ~ glue('Mari szívesen {lemma_orth}. Ők is szívesen...,'),
      tag == 'Prs.NDef.2Sg (mondas)' ~ glue('Mari szívesen {lemma_orth}. Te is szívesen...,')
    ),
    target_words = list(c(v_form,nv_form))
  ) |> 
  select(lemma_orth,tag,prompt,target_words)

# -- write -- #

exp |> 
  toJSON(pretty = TRUE) |> 
  write_lines('dat/mondasz.js')

exp |> 
  toJSON(pretty = TRUE) |> 
  write_lines('~/Github/Pavlovia/verb_task/mondasz.js')
