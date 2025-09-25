################################################
# create training data for SVM
# 1 set up training which is stable cc and vc class and test class which is variable cc verbs
# 2 pair up words across matching xpostags (since we fit sep model for each xpostag)
# 3 create massive list, save
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(stringdist)

# -- fun -- #

buildMDS = function(dist){
  
  dat_matrix_edit = dist |>
    select(test,training,dist) |>
    pivot_wider(names_from = test, values_from = dist) |>
    select(-training) |>
    as.matrix()
  
  mds_edit = stats::cmdscale(dat_matrix_edit, k = 2)
  
  mds_table = tibble(
    transcription = unique(dist$training),
    x_edit = mds_edit[,1],
    y_edit = mds_edit[,2]
  )
}

# -- read -- #

d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- wrangle -- #

my_vowel = '[aáeéiíoóöőuúüű]'

forms = d |> 
  distinct(lemma,lemma_orth,class,varies,form_v,form_nv,tag)

lemmata = d |> 
  distinct(lemma,lemma_orth,class,varies,form_v,form_nv) |> 
  mutate(form = lemma) |> 
  mutate(tag = 'Prs.NDef.3Sg (mond)')

all_forms = bind_rows(forms,lemmata)

cc_forms = all_forms |> 
  filter(class == 'cc', !varies) |> 
  mutate(form = ifelse(
    is.na(form_v),
    form_nv,
    form_v
  )) |> 
  distinct(class,lemma_orth,tag,form) |> 
  rename(
    lemma_training = lemma_orth,
    tag_training = tag,
    training = form
    )

vc_forms = all_forms |> 
  filter(class == 'vc', !varies) |> 
  mutate(form = ifelse(
    is.na(form_nv),
    form_v,
    form_nv
  )) |> 
  distinct(class,lemma_orth,tag,form) |> 
  rename(
    lemma_training = lemma_orth,
    tag_training = tag,
    training = form
  )

target_forms = all_forms |> 
  filter(class == 'cc', varies) |> 
  mutate(form = ifelse(
    is.na(form_nv),
    form_v,
    form_nv
  )) |> 
  distinct(lemma_orth,tag,form) |> 
  rename(
    tag_test = tag,
    test = form
  )

cc_dists = crossing(
    target_forms,
    cc_forms
  ) |> 
  filter(tag_training == tag_test)

vc_dists = crossing(
  target_forms,
  vc_forms
) |> 
  filter(tag_training == tag_test)

dists = bind_rows(vc_dists,cc_dists) |> 
  mutate(
    dist = stringdist(training,test, method = 'lv')
  )

# -- draw distances -- #

dists_nested = dists |> 
  nest(.by = tag_test)

dist = dists_nested$data[[1]]

# -- write -- #

write_tsv(dists, 'dat/distances.gz')
