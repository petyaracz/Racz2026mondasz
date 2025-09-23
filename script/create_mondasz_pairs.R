# create mondasz/mondsz pairs

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)

# -- read -- #

d = read_tsv('dat/webcorpus2_xpostag_v_hunspell_filtered_form.gz')

# -- wrangle -- #

# relevant cells according to Rebrus nagydoktori:
# mondasz, mondanak, mondotok, mondalak, mondtak, mondtam, mondta
# grab tags
my_postags = d |> 
  filter(form %in% c('mondasz', 'mondanak', 'mondotok', 'mondalak', 'mondtak', 'mondtam', 'mondta')) |> 
  pull(xpostag) |> 
  unique()

# find verbs that end in CC (where second C might be sz/zs), keep our postags, create suffix, stem, lemma-final c1c2, detect linking vowel
d2 = d |> 
  filter(
    str_detect(lemma, '[rtpsdfghjklzcvbnm][rtpsdfghjklzcvbnm]$'),
    str_detect(lemma, '[aáeéiíoóöőuúüű](sz|zs)$', negate = T),
    xpostag %in% my_postags
    ) |> 
  add_count(lemma,xpostag) |>
  filter(n > 1) |>
  mutate(
    suffix = str_replace(form, lemma, ''),
    stem = str_replace(form, suffix, ''),
    linking_vowel = ifelse(
      str_detect(suffix, '^[aeouüö](t.k|sz|n.k|t.k|l.k|t.m|t.|tt.k|tt.m|tt.)$'),
      T,F
    ),
    coda = str_extract(lemma, '(?<=[aáeéiíoóöőuúüű])[^aáeéiíoóöőuúüű]+$'),
    c1 = str_extract(coda, '^(sz|[dnlzrjgms])'),
    c2 = str_remove(coda, c1)
  )

# forms w/ linking vowel
d3a = d2 |> 
  filter(linking_vowel) |> 
  select(lemma,xpostag,form,suffix,freq,lfpm10) |> 
  rename(
    form_v = form,
    freq_v = freq,
    lfpm10v = lfpm10,
    suffix_v = suffix
  )

# forms w/o linking vowel
d3b = d2 |> 
  filter(!linking_vowel) |> 
  select(lemma,xpostag,suffix,form,freq,lfpm10) |> 
  rename(
    form_nv = form,
    freq_nv = freq,
    lfpm10nv = lfpm10,
    suffix_nv = suffix
  )

# lemma-level information
d3c = d2 |> 
  distinct(lemma,xpostag,llfpm10,lemma_freq,coda,c1,c2,lemma_syl_count)

# join them up, drop weird ones
d4 = d3c |> 
  full_join(d3a) |> 
  full_join(d3b) |> 
  filter(!lemma %in% c('gyűjt',
                       'kevesell',
                       'kicsinyell')
         ) |> 
  mutate( # calc odds and log odds
    odds_v = freq_v / freq_nv,
    lo_v = log(odds_v)
  )

# make nice labels that tell you that afdauieihof2psgae is the "mondasz" one
labels = d4 |> 
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
d5 = left_join(d4,labels) |> 
  add_count(tag, name = 'tag_freq_in_dataset')

# -- write -- #

write_tsv(d5, 'dat/mondasz_mondsz_webcorpus.tsv')

# esküd-sz ~ esküsz-öl 


