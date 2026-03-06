################################################
# count various ngrams for variable data
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(glue)
library(duckdb)
library(data.table)
library(stringi)

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

# for each string in variable grams
# for all of c2
# count unique lemmas that have this gram and put this in within_count
# count unique forms that have this gram, subtract lemma_count (this gives us the counts for the gram which are not within the word), put this in across_count
# for c2 where verb is T
# do the same
# then
# return within_count and across_count for verb==F (called "all") and verb==T (called "verb") for each gram
count_grams <- function(c2, grams) {
  
  # Precompute unique lemma and form sets (all + verb only)
  lemmas_all  <- unique(c2$lemma_ipa)
  lemmas_verb <- unique(c2[verb == TRUE, lemma_ipa])
  
  results <- rbindlist(lapply(grams, function(g) {
    # All
    within_all  <- sum(stri_detect_fixed(lemmas_all, g))
    
    # Verb only
    within_verb <- sum(stri_detect_fixed(lemmas_verb, g))
    
    data.table(
      gram         = g,
      within_all   = within_all,
      within_verb  = within_verb
    )
  }))
  
  return(results)
}

my_vowel = '[aáeéiíoóöőuúüű]'
my_consonant = '[^aáeéiíoóöőuúüű]'

# -- read -- #

# variable forms
d = read_tsv('dat/mondsz_webcorpus.tsv')

# verbs from Hungarian Webcorpus 2

con = dbConnect(duckdb())

# Set memory limit explicitly
dbExecute(con, "SET memory_limit='2GB'")

c = tbl(con, sql("SELECT * FROM read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet')")) |>
  filter(hunspell, freq > 1) |> 
  group_by(form) |> 
  slice_max(freq, n = 1) |> 
  collect() # collect results

# -- grab grams -- #

grams = unique(na.omit(c(unique(d$trigram),unique(d$bigram1),unique(d$bigram2))))

# -- grab forms -- #

exclude_lemmas = unique(d$lemma)

# -- set up c -- #

setDT(c)

# Deduplicate: transcribe only unique lemmas and forms
unique_lemmas <- unique(c$lemma)
unique_forms  <- unique(c$form)

lemma_map <- data.table(
  lemma     = unique_lemmas,
  lemma_ipa = transcribeIPA(unique_lemmas, 'single')
)

form_map <- data.table(
  form     = unique_forms,
  form_ipa = transcribeIPA(unique_forms, 'single')
)

# Join back, compute suffix and annotated form
c2 <- c[lemma_map, on = "lemma", lemma_ipa := i.lemma_ipa
][form_map, on = "form", form_ipa := i.form_ipa
][, suffix := stri_replace_first_fixed(form_ipa, lemma_ipa, '')
][, annotated_form := paste0(lemma_ipa, '_', suffix)]

c2[, verb := stri_detect_regex(xpostag, '^\\[/V\\]')]
c2 <- c2[!lemma %chin% exclude_lemmas]

rm(c);gc()

# -- find ngrams -- #

result <- count_grams(c2, grams) |> 
  as_tibble()

d2 <- d |>
  left_join(result, by = c("trigram" = "gram")) |>
  rename_with(~ paste0("trigram_", .), within_all:within_verb) |>
  left_join(result, by = c("bigram1" = "gram")) |>
  rename_with(~ paste0("bigram1_", .), within_all:within_verb) |>
  left_join(result, by = c("bigram2" = "gram")) |>
  rename_with(~ paste0("bigram2_", .), within_all:within_verb)

# -- save d with ngram data -- #

write_tsv(d2, 'dat/mondsz_webcorpus_ngram_counts.tsv')
