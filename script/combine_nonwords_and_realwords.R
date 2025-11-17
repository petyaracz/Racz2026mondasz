setwd('~/Github/Racz2026mondasz/')

library(tidyverse)

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

# -- read -- #

mondasz = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing', sheet = 'mondasz_peti') # my annotations based on H's annotations
mondanak = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing', sheet = 'mondanak_heni') # here annotations don't matter I just need the forms
mondtok = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing', sheet = 'mondtok_heni') # same here
mondalak = googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing', sheet = 'mondalak_heni') # same here
real_words = read_tsv('dat/mondasz_training.tsv')

# -- filter and combine -- #

mondasz |> 
  mutate(keep = Pety == 'x' | Heni == 'x') |> 
  filter(keep) |> 
  count(coda)

nonwords = mondasz |> 
  mutate(keep = Pety == 'x' | Heni == 'x') |> 
  filter(keep) |> 
  select(orthography,coda,form_2sg_1,form_2sg_2)

nonwords = mondanak |> 
  select(orthography,form_3pl_1,form_3pl_2) |> 
  right_join(nonwords)

nonwords = mondtok |> 
  select(orthography,form_2pl_1,form_2pl_2) |> 
  right_join(nonwords)

nonwords = mondalak |> 
  select(orthography,form_1sg2_1,form_1sg2_2) |> 
  right_join(nonwords)

# na = count(nonwords,orthography) |>
#   filter(n > 1) |>
#   pull(orthography)
# nonwords |> 
#   filter(orthography %in% na)

nonwords = distinct(nonwords) |>  # shrug emoji
  mutate(lemma = transcribeIPA(orthography, 'single')) |> 
  rename(lemma_orth = orthography)
# count(nonwords,coda)

nl = nonwords |> 
  select(lemma)

rl = real_words |> 
  distinct(lemma)

ll = bind_rows(nl,rl)

# -- write -- #

write_tsv(nonwords, 'dat/nonwords_first_pass.tsv')
