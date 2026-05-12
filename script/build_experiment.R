# use generated and hand-picked nonwords to create forced choice exp

# -- head -- #

library(tidyverse)
library(glue)
library(jsonlite)   # for writing JSON output to Pavlovia

setwd('~/Github/Racz2026mondasz/')

# -- read -- #

d = read_tsv('dat/nonwords_final.tsv')   # finalised nonword stimuli with both V and NV forms

# -- setup -- #

# one list for everyone, easy peasy

# pivot to long so each row is one form (V or NV) for one lemma x inflectional slot combination
dl = d |>
  pivot_longer(-c(lemma, lemma_orth, coda)) |>   # all form columns go long; lemma identifiers stay wide
  mutate(
    tag = case_when(                              # recover the inflectional slot from the column name
      str_detect(name, 'form_1sg2') ~ "Prs.1Sg›2 (mondalak)",
      str_detect(name, 'form_2sg')  ~ "Prs.NDef.2Sg (mondas)",
      str_detect(name, 'form_2pl')  ~ "Prs.NDef.2Pl (mondotok)",
      str_detect(name, 'form_3pl')  ~ "Prs.NDef.3Pl (mondanak)"
    ),
    type = case_when(                             # recover whether this is the NV or V form from the column suffix
      str_detect(name, '_1$') ~ 'nv_form',
      str_detect(name, '_2$') ~ 'v_form'
    )
  ) |>
  select(-name, -coda, -lemma) |>                # drop columns no longer needed after extraction
  pivot_wider(names_from = type, values_from = value)   # back to one row per lemma x slot, with nv_form and v_form side by side

# add experiment-relevant columns: a sentence prompt and the two target words as a list
exp = dl |>
  rowwise() |>
  mutate(
    # sentence completion prompt: fixes 'Mari' as the subject doing the action (3Sg),
    # then sets up the incomplete sentence for the target person/number
    prompt = case_when(
      tag == 'Prs.1Sg›2 (mondalak)'    ~ glue('Mari szívesen {lemma_orth}. Téged én is szívesen...,'),  # 1Sg>2: "I do it to you"
      tag == 'Prs.NDef.2Pl (mondotok)' ~ glue('Mari szívesen {lemma_orth}. Ti is szívesen...,'),        # 2Pl:   "you (pl.) do it"
      tag == 'Prs.NDef.3Pl (mondanak)' ~ glue('Mari szívesen {lemma_orth}. Ők is szívesen...,'),        # 3Pl:   "they do it"
      tag == 'Prs.NDef.2Sg (mondas)'   ~ glue('Mari szívesen {lemma_orth}. Te is szívesen...,')         # 2Sg:   "you (sg.) do it"
    ),
    target_words = list(c(v_form, nv_form))   # bundle both forms as a list for the forced-choice display; order randomised at presentation
  ) |>
  select(lemma_orth, tag, prompt, target_words)

# -- write -- #

# write to local dat/ for archiving and to the Pavlovia experiment repo for deployment
exp |>
  toJSON(pretty = TRUE) |>
  write_lines('dat/mondasz.js')

exp |>
  toJSON(pretty = TRUE) |>
  write_lines('~/Github/Pavlovia/verb_task/mondasz.js')