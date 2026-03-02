################################################
# for dataset, find frequencies of lemmata ending in bigram
# find forms + suffixes ending in bigram
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
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

my_vowel = '[aáeéiíoóöőuúüű]'
my_consonant = '[^aáeéiíoóöőuúüű]'

# -- read -- #

d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# verbs from Hungarian Webcorpus 2

con = dbConnect(duckdb())

# Set memory limit explicitly
dbExecute(con, "SET memory_limit='2GB'")

v = tbl(con, sql("SELECT * FROM read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet')")) |>
  filter(str_detect(xpostag, '^\\[\\/V\\]')) |> 
  collect() # collect results

# -- count ngrams -- #

# trigrams
unique(d$trigram)

# lemma

d |> 
  select()