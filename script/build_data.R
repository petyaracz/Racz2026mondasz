################################################
# create cc verbs, vc verbs, mondasz/mondsz pairs
# 1 pull in verb list from webcorpus 2, clean it up
# 2 split to cc and cvc set, create pairs
# 3 add extra info: nice tag, training cols
# 4 push the whopper button
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(glue)

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

my_vowel = '[aáeéiíoóöőuúüű]'
my_consonant = '[^aáeéiíoóöőuúüű]'

# -- read -- #

# verbs from Hungarian Webcorpus 2
v = read_tsv('https://github.com/petyaracz/Racz2024/raw/main/resource/webcorpus2freqlist/verb_forms.tsv.gz')

# -- wrangle -- #

# relevant cells according to Rebrus nagydoktori:
# mondasz, mondanak, mondotok, mondalak, mondtak, mondtam, mondta
# grab tags
my_postags = v |> 
  filter(form %in% c('mondasz', 'mondanak', 'mondotok', 'mondalak', 'mondtak', 'mondtam', 'mondta')) |> 
  pull(xpostag) |> 
  unique()

# filter for tags and non-ik verbs, create categories of "vc and cc"
v2 = v |>
  filter(hunspell) |> 
  filter(
    lemma != form,
    xpostag %in% my_postags,
    str_detect(form,lemma),
    str_detect(lemma, '(van|lesz|nincs|jön|vesz|tesz|bán|vonz|nemz)$', negate = T)
  ) |> 
  rename(
    form_orth = form,
    lemma_orth = lemma
  ) |> 
  mutate(
    form = transcribeIPA(form_orth, 'single'),
    lemma = transcribeIPA(lemma_orth, 'single'),
    class = ifelse(
      str_detect(lemma, glue('{my_vowel}{my_consonant}$')),
      'vc',
      'cc'
    ),
    suffix = str_remove(form, lemma),
    stem = str_remove(form, suffix),
    linking_vowel = ifelse(
      str_detect(suffix, '^[aeouüö](t.k|s|n.k|t.k|l.k|t.m|.t.|tt.k|tt.m|tt.)$'),
      T,F
    ),
    coda = str_extract(lemma, glue('(?<={my_vowel}){my_consonant}+$')),
    c1 = str_extract(coda, '^.'),
    c2 = str_remove(coda, c1)
  )

# tidying
v3 = v2 |> 
  filter(hunspell::hunspell_check(form_orth, dict = hunspell::dictionary("hu_HU")))

# some eyeballing makes me conclude setdiff v2 v3 is mostly trash, we go with v3

# forms w/ linking vowel
d1a = v3 |> 
  filter(linking_vowel) |> 
  select(lemma,xpostag,form,form_orth,suffix,freq,lfpm10) |> 
  rename(
    form_v_orth = form_orth,
    form_v = form,
    freq_v = freq,
    lfpm10v = lfpm10,
    suffix_v = suffix
  )

# forms w/o linking vowel
d1b = v3 |> 
  filter(!linking_vowel) |> 
  select(lemma,xpostag,suffix,form,form_orth,freq,lfpm10) |> 
  rename(
    form_nv_orth = form_orth,
    form_nv = form,
    freq_nv = freq,
    lfpm10nv = lfpm10,
    suffix_nv = suffix
  )

# lemma-level information
d1c = v3 |> 
  distinct(lemma,lemma_orth,xpostag,llfpm10,lemma_freq,coda,c1,c2,lemma_syl_count,class)

# join them up, drop weird ones
d2 = d1c |> 
  full_join(d1a) |> 
  full_join(d1b) |> 
  filter(!lemma %in% c('gyűjt',
                       'kevesell',
                       'kicsinyell')
  ) |> 
  mutate( # calc odds and log odds
    odds_v = freq_v / freq_nv,
    lo_v = log(odds_v),
    varies = !is.na(form_v) & !is.na(form_nv)
  )

# make nice labels that tell you that afdauieihof2psgae is the "mondasz" one
labels = d2 |> 
  filter(lemma == 'mond') |> 
  distinct(xpostag,form_v) |> 
  mutate(
    clean = xpostag |> 
      str_remove('\\[\\/V\\]') |> 
      str_remove('\\[') |> 
      str_remove('\\]'),
    tag = glue::glue('{clean} ({form_v})')
  ) |> 
  select(xpostag,tag)

# add labels, add tag freq in data
d3 = left_join(d2,labels) |> 
  add_count(tag, name = 'tag_freq_in_dataset')

# define "form" for distances

d4 = d3 |> 
  mutate(
    category = case_when(
      varies & class == 'cc' ~ 'test',
      !varies & class == 'cc' ~ 'cc training',
      !varies & class == 'vc' ~ 'vc training',
    ),
    form1 = ifelse(
      is.na(form_v),
      form_nv,
      form_v
    ),
    form2 = ifelse(
      is.na(form_nv),
      form_v,
      form_nv
    ),
    form = ifelse(
      category == 'vc training',
      form1,
      form2
    )
  ) |> 
  select(-form1,-form2)

# -- write -- #

write_tsv(d4, 'dat/mondasz_mondsz_webcorpus.tsv')
