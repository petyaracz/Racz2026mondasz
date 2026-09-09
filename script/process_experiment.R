# process experiment (https://gitlab.pavlovia.org/petyaraczbme/mondasz)

# -- head -- #

setwd('~/Github/Racz2026mondasz/')
library(tidyverse)
library(glue)

# -- fun -- #

# pivot nonword stimulus table to long format for merging with response data
# same logic as in the stimulus-generation script: recovers inflectional slot and form type from column names
longer = function(dat){
  dat |>
    pivot_longer(-c(lemma, lemma_orth, coda)) |>
    mutate(
      tag = case_when(
        str_detect(name, 'form_1sg2') ~ "Prs.1Sg›2 (mondalak)",
        str_detect(name, 'form_2sg')  ~ "Prs.NDef.2Sg (mondas)",
        str_detect(name, 'form_2pl')  ~ "Prs.NDef.2Pl (mondotok)",
        str_detect(name, 'form_3pl')  ~ "Prs.NDef.3Pl (mondanak)"
      ),
      type = case_when(
        str_detect(name, '_1$') ~ 'nv_form',
        str_detect(name, '_2$') ~ 'v_form'
      )
    ) |>
    select(-name) |>
    pivot_wider(names_from = type, values_from = value)
}

# extract tidy response data and participant metadata from a single raw Pavlovia CSV
gatherData = function(dat){
  
  ## meta
  
  # extract device/screen info if present; fall back to placeholder strings if columns missing
  my_screen_height = ifelse(
    "screen_height" %in% names(dat),
    unique(dat$screen_height[!is.na(dat$screen_height)]),
    'no height info'
  )
  my_screen_width = ifelse(
    "screen_width" %in% names(dat),
    unique(dat$screen_width[!is.na(dat$screen_width)]),
    'no width info'
  )
  my_agent = ifelse(
    "user_agent" %in% names(dat),
    unique(dat$user_agent[!is.na(dat$user_agent)]),
    'no agent info'
  )
  my_device = ifelse(
    "device_type" %in% names(dat),
    unique(dat$device_type[!is.na(dat$device_type)]),
    'no device info'
  )
  
  user_info = glue('device: {my_device}, screen width: {my_screen_width}, screen height: {my_screen_height}, user info: {my_agent}')
  
  # extract survey responses: participant ID, gender, year of birth
  # Pavlovia stores survey-text responses as a JSON string; regex into it
  survey_text = dat |>
    filter(trial_type == 'survey-text') |>
    pull(response)
  
  raw_id = str_extract(survey_text, '(?<="Q0":")[^"]+')   # Q0: participant ID
  gender = str_extract(survey_text, '(?<="Q1":")[^"]+')   # Q1: gender
  yob    = str_extract(survey_text, '(?<="Q2":")[^"]+')   # Q2: year of birth
  
  # score attention/comprehension checks: rows where stimulus starts with a question word
  check_answers = dat |>
    filter(str_detect(stimulus, '^(Ki|Mi|Hány|Milyen|Hol)')) |>   # Hungarian wh-words mark check trials
    select(stimulus, response_string)
  
  # join against the correct answer key and count how many the participant got right
  got_right = left_join(correct_answers, check_answers, by = join_by(stimulus)) |>
    mutate(correct = response_string == correct_response) |>
    count(correct) |>
    filter(correct) |>
    pull(n)
  
  checks_passed = glue('{got_right} / 8')   # format as "n / 8" for readability
  
  ## resp
  
  # extract experimental trials: rows where stimulus starts with 'Mari' (the sentence-completion prompt)
  resp = dat |>
    filter(str_detect(stimulus, '^Mari')) |>
    mutate(
      # recover lemma from the first clause of the prompt ("Mari szívesen <lemma>. ")
      lemma_orth = str_extract(stimulus, '(?<=szívesen )[^.]*(?=\\. )'),
      # recover inflectional slot from the second clause of the prompt
      tag = case_when(
        str_detect(stimulus, 'Téged én') ~ '1Sg›2 (rajongalak)',
        str_detect(stimulus, 'Ti is')    ~ '2Pl (rajongotok)',
        str_detect(stimulus, 'Ők is')    ~ '3Pl (rajonganak)',
        str_detect(stimulus, 'Te is')    ~ '2Sg (rajongasz)'
      )
    ) |>
    select(lemma_orth, tag, response_string, rt, trial_index, time_elapsed) |>
    mutate(
      rt            = as.double(rt),
      gender        = gender,
      yob           = yob,
      checks_passed = checks_passed,
      raw_id        = raw_id,
      user_info     = user_info,
      total_time    = sum(rt)   # total experiment time in ms (sum of per-trial RTs)
    )
  
  return(resp)
}

# -- read -- #

s               = read_tsv('dat/nonwords_final.tsv')              # nonword stimulus table
correct_answers = read_tsv('dat/correct_answers_to_checks.tsv')   # answer key for comprehension checks

# -- collect responses -- #

sl = s |> longer()   # pivot stimulus table for merging

my_path   = '~/Gitlab/mondasz/data/'
file_name = list.files(my_path)

# read all participant CSVs matching the expected filename pattern, apply gatherData to each
d = tibble(file_name = file_name) |>
  filter(str_detect(file_name, 'mondasz_mondasz_participant')) |>   # exclude test runs and other files
  mutate(
    raw  = map(file_name, ~ read_csv(glue("{my_path}{.}"))),        # read raw CSV per participant
    tidy = map(raw, gatherData)                                      # extract tidy responses per participant
  ) |>
  select(file_name, tidy) |>
  unnest(tidy) |>                  # one row per trial per participant
  left_join(sl) |>                 # attach stimulus info (v_form, nv_form, coda, etc.)
  mutate(
    resp_v   = response_string == v_form,              # TRUE if participant chose the V-form
    rt_upper = median(rt) + 3 * mad(rt)               # upper RT cutoff (median + 3 MAD); applied downstream
  )

# -- print info -- #

# count participants who passed enough comprehension checks (6, 7, or 8 out of 8)
n_passed = d |>
  distinct(raw_id, checks_passed) |>
  filter(checks_passed %in% c('8 / 8', '7 / 8', '6 / 8')) |>
  nrow()

n_total = length(unique(d$raw_id))

glue('> {n_passed} / {n_total} participants passed checks.')   # quick console summary

ids = d |>
  filter(checks_passed %in% c('8 / 8', '7 / 8', '6 / 8')) |>   # keep only participants who passed comprehension checks
  distinct(raw_id) |>
  arrange(raw_id)   # sorted participant ID list (for record-keeping / GDPR log)

# -- write -- #

write_tsv(d,   'dat/exp_data_tidy.tsv.gz')   # full tidy dataset
write_tsv(ids, 'dat/passed.tsv')             # participant ID list