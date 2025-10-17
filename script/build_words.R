# -- head -- #

library(tidyverse)
library(glue)

setwd('~/Github/Racz2026mondasz/')

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', 's' = 'š', 'ß' = 's', 'x' = 'ks'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs', 'cscs' = 'ccs', 'szsz' = 'ssz', 'zszs' = 'zzs', 'tyty' = 'tty', 'gygy' = 'ggy', 'nyny' = 'nny'))
  }
}

# take words, coda, generate non-word
makeWord = function(words, my_coda = 'ng'){
  
  my_onset = words$onset[sample(nrow(words), 1)]
  my_vowel = words$vowel1[sample(nrow(words), 1)]
  my_middle = words$middle[sample(nrow(words), 1)]
  my_link = case_when(
    my_vowel %in% c('e','é','i','í') ~ 'e',
    my_vowel %in% c('ö','ő','ü','ű') ~ 'ö',
    my_vowel %in% c('u','ú','o','ó','a','á') ~ 'o'
  )
  paste0(my_onset, my_vowel, my_middle, my_link, my_coda)
}

# -- read -- #

# variable words for reference
# d = read_tsv('dat/mondasz_target.tsv')
# verbs from webcorpus
c = read_tsv('~/Github/Racz2024/resource/webcorpus2freqlist/verb_forms.tsv.gz')

# -- subset -- #

# chuck out sus verbs
c2 = c |> 
  filter(hunspell::hunspell_check(form, dict = hunspell::dictionary('hu_HU')))

# keep canonical forms, 2 syl, set up cols
c3 = c2 |> 
  filter(
    xpostag == '[/V][Prs.NDef.3Sg]',
    form_syl_count == 2,
    freq > 1,
    str_detect(form, '[őűú].$', negate = T),
    str_detect(form, 'ik$', negate = T),
    str_detect(form, '^(ch|be|el|fel|föl|ki|le|meg|rá|át|túl)', negate = T) # common preverbs dropped
    ) |> 
  distinct(form,lemma_freq,lemma_syl_count) |> 
  mutate(
    verb = transcribeIPA(form, 'single'),
    freq = lemma_freq,
    nsyl = lemma_syl_count
         ) |> 
  select(verb,freq,nsyl)

# generate words list which has constituents (onset, first vowel, middle bit)
# we don't need the end of the word since we make that up anyway
words = c3 |> 
  mutate(
    onset = ifelse(
      str_detect(verb, '^[aáeéiíoóöőuúüű]'),
      '',
      str_extract(verb, '[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')
    ),
    vowel1 = ifelse(
      onset == '',
      str_extract(verb, '^[aáeéiíoóöőuúüű]'),
      str_extract(verb, glue('(?<={onset})[aáeéiíoóöőuúüű]'))
    ),
    middle = ifelse(
      str_detect(verb, '[aáeéiíoóöőuúüű][aáeéiíoóöőuúüű]'),
      '',
      str_extract(verb, glue('(?<={onset}{vowel1})[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'))
    )
  )

# -- generate words --#

# for each coda, make n nonwords, turn back into ortography, extract stem
n = 250

targets = tibble(
  coda = c(rep('ng', n),rep('nt', n),rep('jt', n),rep('st', n))
) |>
  rowwise() |> 
  mutate(
    target = map_chr(coda, ~ makeWord(words, my_coda = .)),
    stem = str_remove(target, coda),
    orthography = transcribeIPA(target, 'double')
  )

# cross-ref

lexicon = c2 |> 
  distinct(lemma) |> # speed it up
  mutate(lexicon = transcribeIPA(lemma, 'single')) |> 
  filter(nchar(lexicon) < 8) |> 
  select(lexicon)

filtered_targets = targets |> 
  select(stem) |> 
  crossing(lexicon) |> 
  mutate(lv = stringdist::stringdist(stem, lexicon, method = 'lv')) |> 
  summarise(min_lv = min(lv), .by = stem) |> 
  filter(min_lv > 1)

# -- final -- #

final = targets |> 
  filter(stem %in% filtered_targets$stem) 

# -- add forms -- #

final_forms = final |> 
  mutate(
    v = str_extract(stem, '[eöo]$'),
    form_2sg_1 = glue('{orthography}sz'),
    form_2sg_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}esz'),
      v == 'o' ~ glue('{orthography}asz')
    ),
    form_3pl_1 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}nek'),
      v == 'o' ~ glue('{orthography}nak')
    ),
    form_3pl_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}enek'),
      v == 'o' ~ glue('{orthography}anak')
    ),
    form_1sg2_1 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}lek'),
      v == 'o' ~ glue('{orthography}lak')
    ),
    form_1sg2_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}elek'),
      v == 'o' ~ glue('{orthography}alak')
    ),
    form_2pl_1 = case_when(
      v == 'e' ~ glue('{orthography}tek'),
      v == 'ö' ~ glue('{orthography}tök'),
      v == 'o' ~ glue('{orthography}tok')
    ),
    form_2pl_2 = case_when(
      v == 'e' ~ glue('{orthography}etek'),
      v == 'ö' ~ glue('{orthography}ötök'),
      v == 'o' ~ glue('{orthography}otok')
    )
  ) |> 
  filter(!is.na(v))
  
my_url = 'https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing'

final_forms[sample(1:nrow(final_forms)), ] |> 
  select(coda,orthography,form_2sg_1,form_2sg_2) |> 
  googlesheets4::write_sheet(my_url, 'mondasz')

final_forms[sample(1:nrow(final_forms)), ] |> 
  select(coda,orthography,form_3pl_1,form_3pl_2) |> 
  googlesheets4::write_sheet(my_url, 'mondanak')

final_forms[sample(1:nrow(final_forms)), ] |> 
  select(coda,orthography,form_1sg2_1,form_1sg2_2) |> 
  googlesheets4::write_sheet(my_url, 'mondalak')

final_forms[sample(1:nrow(final_forms)), ] |> 
  select(coda,orthography,form_2pl_1,form_2pl_2) |> 
  googlesheets4::write_sheet(my_url, 'mondtok')
