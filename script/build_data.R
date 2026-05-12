################################################
# create cc verbs, vc verbs, mondasz/mondsz pairs from corpus
# 1 pull in verb list from webcorpus 2, clean it up
# 2 split to cc and cvc set, create pairs
# 3 add extra info: nice tag, training cols
# 4 push the whopper button
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/') # set working directory to project root

library(tidyverse) # data manipulation and visualisation
library(glue) # string interpolation
library(duckdb) # fast in-process SQL engine for reading parquet files

# -- fun -- #

# Hungarian orthography: digraphs (cs, sz, zs, ty, gy, ny, ly, s) map onto single characters
# direction = 'single': orthography -> IPA-style single characters (e.g. 'cs' -> 'č', 'sz' -> 's' via 'ß')
# direction = 'double': IPA-style single characters -> orthography (reverse)
# Long digraphs (ccs, ssz, etc.) are expanded first to avoid double-mapping
transcribeIPA = function(string, direction){
 if (direction == 'single'){
 stringr::str_replace_all(string, c(
 'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj', # expand geminate digraphs to double digraph first
 'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j', # map digraphs to single characters
 's' = 'š', 'ß' = 's', # remap s -> š, then ß (formerly sz) -> s, so 'sz' ends up as 's' and 's' as 'š'
 'x' = 'ks')) # expand x to ks
 } else if (direction == 'double'){
 stringr::str_replace_all(string, c('s' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs'))
 }
}

my_vowel = '[aáeéiíoóöőuúüű]' # regex character class for Hungarian vowels
my_consonant = '[^aáeéiíoóöőuúüű]' # regex character class for Hungarian consonants (negated vowels)

# -- read -- #

# verbs from Hungarian Webcorpus 2

con = dbConnect(duckdb()) # open in-process DuckDB connection

dbExecute(con, "SET memory_limit='2GB'") # cap memory use to avoid OOM on large parquet file

# read the frequency parquet, filter to verb lemmas (xpostag starts with [/V]), load into R
v = tbl(con, sql("SELECT * FROM read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet')")) |>
 filter(str_detect(xpostag, '^\\[\\/V\\]')) |>
 collect()

# -- wrangle -- #

# the variation of interest: linking-vowel alternation in verb paradigms
# Rebrus (nagydoktori) identifies these inflectional slots as the key sites:
# mondasz ~ mondsz (Prs.NDef.2Sg), mondanak ~ mondnak (Prs.NDef.3Pl),
# mondalak ~ mondlak (Prs.1Sg>2), mondotok ~ mondtok (Prs.NDef.2Pl)
# mondtam is excluded: ambiguous def/indef
# mondtak excluded: bleeds into 2Sg counts
my_postags = c('[/V][Prs.NDef.2Sg]','[/V][Prs.NDef.3Pl]','[/V][Prs.1Sg›2]','[/V][Prs.NDef.2Pl]')

v2 = v |>
 filter(hunspell) |>   # keep only hunspell-accepted forms
 filter(
 nchar(lemma) > 2,   # minimum length: a VCC verb needs at least 3 chars
 lemma != form,    # drop cases where lemma and form are identical (bare infinitives mislabelled?)
 xpostag %in% my_postags,   # keep only the four target inflectional slots
 str_detect(form, lemma),   # form must contain the lemma string (transparent suffixation)
 str_detect(lemma, '(van|lesz|nincs|jön|vesz|tesz|bán|vonz|nemz)$', negate = T) # exclude suppletive/irregular verbs
 ) |>
 rename(
 form_orth = form, # rename to mark original orthographic form
 lemma_orth = lemma # rename to mark original orthographic lemma
 ) |>
 mutate(
 form = transcribeIPA(form_orth, 'single'), # convert form to single-character IPA for downstream regex
 lemma = transcribeIPA(lemma_orth, 'single') # convert lemma to single-character IPA
 ) |>
 filter(
 str_detect(lemma, glue('{my_vowel}')),  # lemma must contain at least one vowel
 str_detect(lemma, glue('{my_vowel}{my_consonant}$'), negate = T), # exclude VC-final lemmas (already have epenthesis baked in?)
 str_detect(lemma, glue('{my_vowel}$'), negate = T), # exclude V-final lemmas (ik-verb territory; handled separately)
 str_detect(lemma, '(.)\\1$', negate = T),  # exclude lemmas ending in geminate consonant
 )

v3 = v2 |>
 mutate(
 suffix = str_remove(form, lemma), # isolate the suffix by stripping the lemma from the form
 stem = str_remove(form, suffix), # remainder after stripping suffix = stem (should equal lemma if no alternation)
 linking_vowel = ifelse(  # TRUE if suffix starts with a linking vowel before the suffix proper
 str_detect(suffix, '^[aeouüö](t.k|s|n.k|t.k|l.k|t.m|.t.|tt.k|tt.m|tt.)$'),
 T, F
 ),
 coda = str_extract(lemma, glue('(?<={my_vowel}){my_consonant}+$')), # extract consonant cluster after last vowel in lemma (the coda)
 coda = ifelse(is.na(coda), '', coda), # replace NA coda (V-final, shouldn't survive filters) with empty string
 c1 = str_extract(coda, '^.'),  # first consonant of coda
 c2 = str_extract(coda, '.$')  # last consonant of coda (= segment immediately before suffix)
 )

# hunspell-filter the forms (orthographic), dropping anything hunspell rejects
v4 = v3 |>
 filter(hunspell::hunspell_check(form_orth, dict = hunspell::dictionary("hu-HU")))

setdiff(v3$form, v4$form) # inspect dropped forms; comment in script: mostly noise but some loss

# -- build form pairs: linking-vowel forms (d1a) vs. no-linking-vowel forms (d1b) --

# linking-vowel forms: rename frequency and form columns to mark the V-variant
d1a = v4 |>
 filter(linking_vowel) |>
 select(lemma, xpostag, form, form_orth, suffix, freq, lfpm10) |>
 rename(
 form_v_orth = form_orth,
 form_v = form,
 freq_v = freq,
 lfpm10v = lfpm10,
 suffix_v = suffix
 )

# no-linking-vowel forms: rename frequency and form columns to mark the NV-variant
d1b = v4 |>
 filter(!linking_vowel) |>
 select(lemma, xpostag, suffix, form, form_orth, freq, lfpm10) |>
 rename(
 form_nv_orth = form_orth,
 form_nv = form,
 freq_nv = freq,
 lfpm10nv = lfpm10,
 suffix_nv = suffix
 )

# -- recover NV forms that exist in the corpus but didn't pass the linking_vowel filter --
# for each V-form, construct what the corresponding NV-form would look like by removing the linking vowel
possible_nv_forms = d1a |>
 mutate(
 form = case_when(
 xpostag == '[/V][Prs.1Sg›2]' ~ str_remove(form_v, '.(?=l.k$)'), # mondalak -> mondlak
 xpostag == '[/V][Prs.NDef.2Pl]' ~ str_remove(form_v, '.(?=t.k$)'), # mondotok -> mondtok
 xpostag == '[/V][Prs.NDef.2Sg]' ~ str_remove(form_v, '.(?=s$)'), # mondasz -> mondsz
 xpostag == '[/V][Prs.NDef.3Pl]' ~ str_remove(form_v, '.(?=n.k$)'), # mondanak -> mondnak
 )
 ) |>
 select(lemma, xpostag, form)

# look up these constructed NV forms in v3 (pre-hunspell-filter, to not lose attested forms)
found_nv_forms = v3 |>
 inner_join(possible_nv_forms) |>
 select(lemma, xpostag, form, form_orth, suffix, freq, lfpm10) |>
 rename(
 form_nv_orth = form_orth,
 form_nv = form,
 freq_nv = freq,
 lfpm10nv = lfpm10,
 suffix_nv = suffix
 )

# add recovered NV forms back into d1b
d1b = d1b |>
 bind_rows(found_nv_forms)

# lemma-level information (one row per lemma x postag, with coda structure etc.)
d1c = v4 |>
 distinct(lemma, lemma_orth, xpostag, llfpm10, lemma_freq, coda, c1, c2, lemma_syl_count)

# join lemma info, V-forms, and NV-forms; drop manually identified problematic lemmas
d2 = d1c |>
 full_join(d1a) |>
 full_join(d1b) |>
 filter(!lemma %in% c('gyűjt', 'kevesell', 'kicsinyell')) |> # manually excluded: irregular or analytically messy
 mutate(
 odds_v = freq_v / freq_nv, # odds of V-form over NV-form
 lo_v = log(odds_v), # log odds (response variable for modelling)
 varies = !is.na(form_v) & !is.na(form_nv) # TRUE if both variants are attested for this lemma x postag
 )

# make human-readable tag labels, anchored to 'rajong' as the example verb
labels = d2 |>
 filter(lemma_orth == 'rajong') |>  # use verb
 distinct(xpostag, form_v_orth) |>
 mutate(
 clean = xpostag |>
 str_remove('\\[\\/V\\]') |> # strip the [/V] prefix
 str_remove('\\[') |>
 str_remove('\\]') |>   # strip surrounding brackets
 str_remove('Prs\\.') |> # strip everything
 str_remove('NDef\\.'),
 tag = glue::glue('{clean} ({form_v_orth})') # e.g. "2Sg (akarmi)"
 ) |>
 select(xpostag, tag)

# attach labels, count how many lemmas appear per tag in the dataset
d3 = left_join(d2, labels) |>
 add_count(tag, name = 'tag_freq_in_dataset')

# -- duplicates -- #

d3 = d3 |>
 distinct() # drop exact duplicate rows

# -- filter for exp -- #

# see explore_data

# experiment dataset: keep only lemmas where log odds is defined (both forms attested and non-zero)
d4 = d3 |>
 filter(!is.na(lo_v))

# -- filter for corpus stuff -- #

# corpus/modelling dataset: keep only C-final lemmas (proper CC-stem verbs);
# add trigram/bigram features at the stem-suffix boundary for phonological modelling
d5 = d3 |>
 filter(str_detect(lemma, glue('{my_consonant}$'))) |> # lemma must end in consonant (i.e. CC-final after coda)
 mutate(
 suffix_init = ifelse(is.na(suffix_nv), NA, str_extract(suffix_nv, '^.')), # first character of NV suffix
 trigram = ifelse(is.na(suffix_nv), NA, glue('{coda}{suffix_init}')), # coda + suffix onset = trigram at boundary
 bigram1 = coda,     # coda alone (stem-internal bigram)
 bigram2 = ifelse(is.na(suffix_nv), NA, str_extract(trigram, '..$')), # last two characters of trigram (C2 + suffix onset)
 type = case_when(
 is.na(form_nv_orth) ~ 'linking vowel always', # only V-form attested
 is.na(form_v_orth) ~ 'linking vowel never', # only NV-form attested
 T  ~ 'linking vowel varies' # both forms attested
 )
 )

# -- write -- #

write_tsv(d4, 'dat/corpus_cc_varies.tsv') # experiment dataset
write_tsv(d5, 'dat/corpus_cc.tsv') # corpus/modelling dataset

# -- note -- #

# d4 is the experiment dataset. The only filter is !is.na(lo_v), which means both the V-form and the NV-form must be attested with non-zero frequency, so a log-odds ratio is computable. This is the set of verbs where genuine variation is observed in the corpus. It includes verbs with any coda shape, as long as both surface forms show up. No boundary-feature engineering is added.
# d5 is the corpus/modelling dataset. The filter is str_detect(lemma, glue('{my_consonant}$')): lemmas must be C-final (i.e. they have a non-empty coda, properly CC-final after the root vowel). This is the superset d3, not the variation-only d4: it includes lemmas where only the V-form is attested, only the NV-form is attested, or both are attested. The type column makes those three cases explicit. It adds phonological features at the stem-suffix boundary (trigram, bigram1, bigram2) for modelling what phonological context predicts linking-vowel presence.
# So: d4 is for fitting a model of variation given that variation exists; d5 is for the broader analysis of what phonological structure predicts the distribution across all verb types, including categorical ones.