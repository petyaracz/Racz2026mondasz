# generate nonwords for hand-filtering to then be passed into experiment

# -- head -- #

library(tidyverse)
library(glue)

setwd('~/Github/Racz2026mondasz/')

# -- fun -- #

# Hungarian orthography: replace characters in digraphs with their IPA equivalents or vice versa
# direction = 'single': orthography -> single-character IPA (e.g. 'cs' -> 'č', 'sz' -> 's')
# direction = 'double': single-character IPA -> orthography (reverse)
# geminate digraphs expanded first to avoid double-mapping (e.g. 'ccs' -> 'cscs' before 'cs' -> 'č')
transcribeIPA = function(string, direction){
  if (direction == 'single'){
    stringr::str_replace_all(string, c(
      'ccs' = 'cscs', 'ssz' = 'szsz', 'zzs' = 'zszs', 'tty' = 'tyty', 'ggy' = 'gygy', 'nny' = 'nyny', 'lly' = 'jj',
      'cs' = 'č', 'sz' = 'ß', 'zs' = 'ž', 'ty' = 'ṯ', 'gy' = 'ḏ', 'ny' = 'ṉ', 'ly' = 'j',
      's' = 'š', 'ß' = 's', 'x' = 'ks'))
  } else if (direction == 'double'){
    stringr::str_replace_all(string, c(
      's' = 'ß', 'š' = 's', 'ṉ' = 'ny', 'ḏ' = 'gy', 'ṯ' = 'ty', 'ž' = 'zs', 'ß' = 'sz', 'č' = 'cs',
      'cscs' = 'ccs', 'szsz' = 'ssz', 'zszs' = 'zzs', 'tyty' = 'tty', 'gygy' = 'ggy', 'nyny' = 'nny'))  # collapse doubled digraphs back to geminates
  }
}

# generate a single nonce verb with a specified coda
# samples onset, first vowel, and medial consonant cluster independently from real verb forms
# constructs a linking vowel by vowel harmony based on the sampled vowel
# returns the full nonce form as a single-character IPA string
makeWord = function(words, my_coda = 'ng'){
  my_onset  = words$onset[sample(nrow(words), 1)]   # random onset consonant cluster (or empty for V-initial)
  my_vowel  = words$vowel1[sample(nrow(words), 1)]  # random first vowel
  my_middle = words$middle[sample(nrow(words), 1)]  # random medial consonant cluster (between vowels)
  my_link = case_when(                               # linking vowel by vowel harmony
    my_vowel %in% c('e','é','i','í')         ~ 'e', # front unrounded
    my_vowel %in% c('ö','ő','ü','ű')         ~ 'ö', # front rounded
    my_vowel %in% c('u','ú','o','ó','a','á') ~ 'o'  # back
  )
  paste0(my_onset, my_vowel, my_middle, my_link, my_coda)  # assemble: onset + V1 + middle + link + coda
}

# -- read -- #

# variable words for reference (commented out; not used in this run)
# d = read_tsv('dat/mondasz_target.tsv')

# real verb forms from Webcorpus 2 frequency list (used as phonological donors for nonce construction)
c = read_tsv('~/Github/Racz2024/resource/webcorpus2freqlist/verb_forms.tsv.gz')

# -- subset -- #

# drop forms hunspell rejects as non-Hungarian
c2 = c |>
  filter(hunspell::hunspell_check(form, dict = hunspell::dictionary('hu_HU')))

# keep 3Sg present indefinite forms (the canonical citation form for Hungarian verbs),
# restrict to 2-syllable forms with freq > 1, exclude long vowels before final C (phonological oddities),
# exclude ik-verbs (different paradigm), exclude prefixed verbs (preverbs create frequency artefacts)
c3 = c2 |>
  filter(
    xpostag == '[/V][Prs.NDef.3Sg]',
    form_syl_count == 2,
    freq > 1,
    str_detect(form, '[őűú].$', negate = T),                                       # exclude long-vowel-penult forms
    str_detect(form, 'ik$', negate = T),                                            # exclude ik-verbs
    str_detect(form, '^(ch|be|el|fel|föl|ki|le|meg|rá|át|túl)', negate = T)        # exclude prefixed forms
  ) |>
  distinct(form, lemma_freq, lemma_syl_count) |>
  mutate(
    verb = transcribeIPA(form, 'single'),   # convert to single-char IPA for regex operations
    freq = lemma_freq,
    nsyl = lemma_syl_count
  ) |>
  select(verb, freq, nsyl)

# decompose each real verb into its phonological constituents for use as a sampling pool
# onset: consonants before the first vowel (empty string if V-initial)
# vowel1: the first vowel
# middle: consonant cluster between vowel1 and the second vowel (NA if vowels are adjacent)
words = c3 |>
  mutate(
    onset = ifelse(
      str_detect(verb, '^[aáeéiíoóöőuúüű]'),          # V-initial: no onset
      '',
      str_extract(verb, '[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])')  # C+ before first V
    ),
    vowel1 = ifelse(
      onset == '',
      str_extract(verb, '^[aáeéiíoóöőuúüű]'),          # V-initial: first char is vowel1
      str_extract(verb, glue('(?<={onset})[aáeéiíoóöőuúüű]'))       # V after onset
    ),
    middle = ifelse(
      str_detect(verb, '[aáeéiíoóöőuúüű][aáeéiíoóöőuúüű]'),         # adjacent vowels: no middle consonant
      '',
      str_extract(verb, glue('(?<={onset}{vowel1})[^aáeéiíoóöőuúüű]+(?=[aáeéiíoóöőuúüű])'))  # C+ between V1 and V2
    )
  )

# -- generate words -- #

# generate n nonce verbs per coda type; four codas covering the main CC clusters of interest
n = 250

targets = tibble(
  coda = c(rep('ng', n), rep('nt', n), rep('jt', n), rep('st', n))  # 1000 items total
) |>
  rowwise() |>
  mutate(
    target      = map_chr(coda, ~ makeWord(words, my_coda = .)),    # generate nonce form (IPA)
    stem        = str_remove(target, coda),                          # strip coda to get stem (for lexical distance check)
    orthography = transcribeIPA(target, 'double')                    # convert back to Hungarian orthography
  )

# -- filter by lexical distance -- #

# build a lexicon of real verb stems in single-char IPA, restricted to short items (< 8 chars)
# to keep the Levenshtein computation tractable
lexicon = c2 |>
  distinct(lemma) |>
  mutate(lexicon = transcribeIPA(lemma, 'single')) |>
  filter(nchar(lexicon) < 8) |>
  select(lexicon)

# for each nonce stem, compute minimum Levenshtein distance to all real lemmas
# keep only nonce stems that are > 1 edit away from any real word (i.e. not near-homophones of real verbs)
filtered_targets = targets |>
  select(stem) |>
  crossing(lexicon) |>                                                          # all stem x lexicon pairs
  mutate(lv = stringdist::stringdist(stem, lexicon, method = 'lv')) |>         # Levenshtein distance
  summarise(min_lv = min(lv), .by = stem) |>                                   # minimum distance per stem
  filter(min_lv > 1)                                                            # discard near-real-word stems

# keep only nonce verbs whose stem passed the lexical distance filter
final = targets |>
  filter(stem %in% filtered_targets$stem)

# -- add inflected forms -- #

# for each nonce verb, generate both the V-form (with linking vowel) and NV-form (without)
# for all four inflectional slots: 2Sg, 3Pl, 1Sg>2, 2Pl
# vowel harmony is determined by the linking vowel already present at the stem edge
final_forms = final |>
  mutate(
    v = str_extract(stem, '[eöo]$'),          # extract linking vowel from stem edge (determines harmony class)
    # 2Sg: mondsz (NV) vs mondasz (V)
    form_2sg_1 = glue('{orthography}sz'),
    form_2sg_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}esz'),
      v == 'o'          ~ glue('{orthography}asz')
    ),
    # 3Pl: mondnak (NV) vs mondanak (V)
    form_3pl_1 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}nek'),
      v == 'o'          ~ glue('{orthography}nak')
    ),
    form_3pl_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}enek'),
      v == 'o'          ~ glue('{orthography}anak')
    ),
    # 1Sg>2: mondlak (NV) vs mondalak (V)
    form_1sg2_1 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}lek'),
      v == 'o'          ~ glue('{orthography}lak')
    ),
    form_1sg2_2 = case_when(
      v %in% c('e','ö') ~ glue('{orthography}elek'),
      v == 'o'          ~ glue('{orthography}alak')
    ),
    # 2Pl: mondtok (NV) vs mondotok (V); front rounded has its own form
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
  filter(!is.na(v))   # drop any nonce verbs where the linking vowel couldn't be determined

# -- write to Google Sheets -- #

# target spreadsheet
my_url = 'https://docs.google.com/spreadsheets/d/1TxA5IyqPOZvZ8IwpSFuAdixjse6B0acGQvcbIBJNaoU/edit?usp=sharing'

# write each inflectional slot to its own sheet, randomising row order each time
# each sheet has: coda type, nonce orthography, NV-form, V-form
final_forms[sample(1:nrow(final_forms)), ] |>
  select(coda, orthography, form_2sg_1, form_2sg_2) |>
  googlesheets4::write_sheet(my_url, 'mondasz')    # 2Sg sheet

final_forms[sample(1:nrow(final_forms)), ] |>
  select(coda, orthography, form_3pl_1, form_3pl_2) |>
  googlesheets4::write_sheet(my_url, 'mondanak')   # 3Pl sheet

final_forms[sample(1:nrow(final_forms)), ] |>
  select(coda, orthography, form_1sg2_1, form_1sg2_2) |>
  googlesheets4::write_sheet(my_url, 'mondalak')   # 1Sg>2 sheet

final_forms[sample(1:nrow(final_forms)), ] |>
  select(coda, orthography, form_2pl_1, form_2pl_2) |>
  googlesheets4::write_sheet(my_url, 'mondtok')    # 2Pl sheet