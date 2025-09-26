################################################
# create training data for GCM, SVM
# GCM: pairs of test - training items with dist
# SVM: all pairwise distances of test items and labels -> I can turn this into a distance matrix. also: all distances of test items (rows) and all training items (cols) as a matrix, I can use this to predict
# test verbs: varying cc
# training verbs: stable cc and stable cv
# put a filter on it: like, min freq of 10 for training sets
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(stringdist)
library(glue)

# -- read -- #

d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- setup -- #

forms = d |> 
  filter(
    (varies & class == 'cc') | (freq_v > 9 | freq_nv > 9)
  ) |> 
  distinct(lemma,lemma_orth,category,form,tag)
  

# add basic form
lemmata = d |> 
  distinct(lemma,lemma_orth,category,form) |> 
  mutate(form = lemma) |> 
  mutate(tag = 'Prs.NDef.3Sg (mond)')

all_forms = bind_rows(forms,lemmata)

# -- GCM -- #

cc_forms = all_forms |> 
  filter(category == 'cc training') |> 
  distinct(category,lemma_orth,tag,form) |> 
  rename(
    lemma_training = lemma_orth,
    tag_training = tag,
    training = form
    )

vc_forms = all_forms |> 
  filter(category == 'vc training') |> 
  distinct(category,lemma_orth,tag,form) |> 
  rename(
    lemma_training = lemma_orth,
    tag_training = tag,
    training = form
  )

target_forms = all_forms |> 
  filter(category == 'test') |> 
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

gcmdists = bind_rows(vc_dists,cc_dists) |> 
  mutate(
    dist = stringdist(training,test, method = 'lv')
  )

# -- SVM -- #

# for each postag, build training matrix where col1 class col2 training items 1...n, col... training items 1...n
# and build test-training matrix where col1 test items 1...n and col... training items 1...n

unique(all_forms$tag)

buildSVMmatrices = function(all_forms,my_tag){
  my_forms = all_forms |> 
    filter(tag == my_tag)
  
  my_training = my_forms |> 
    filter(category != 'test') |> 
    select(category,form) |> 
    mutate(form2 = lag(form))
  
  my_training[1,]$form2 = my_training[nrow(my_training),]$form # what goes around comes around
  
  my_training = my_training |> 
    mutate(dist = stringdist(form,form2, method = 'lv'))
  
  my_test = my_forms |> 
    filter(category == 'test') |> 
    crossing(training = my_training$form) |> 
    mutate(
      dist = stringdist(form,training, method = 'lv')
    )
  
  list(my_training,my_test)
}

# -- write -- #

write_tsv(gcmdists, 'dat/gcm_distances.gz')

my_tags = unique(all_forms$tag)

for (i in 1:length(my_tags)){

  my_tag = my_tags[i]
  my_list = buildSVMmatrices(all_forms, my_tag)
  my_training = my_list[[1]]
  my_test = my_list[[2]]
  my_name = str_replace_all(my_tag, '[\\. \\(\\)]', '_')
  write_tsv(my_training, glue('dat/svm_training_{my_name}.gz'))
  write_tsv(my_test, glue('dat/svm_test_{my_name}.gz'))
}
