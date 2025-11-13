# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(glue)

# -- fun -- #

# make nonword list long for merge
longer = function(dat){
  dat |> 
  pivot_longer(-c(lemma,lemma_orth,coda)) |> 
  mutate(
    tag = case_when(
      str_detect(name, 'form_1sg2') ~ "Prs.1Sg›2 (mondalak)",
      str_detect(name, 'form_2sg') ~ "Prs.NDef.2Sg (mondas)",
      str_detect(name, 'form_2pl') ~ "Prs.NDef.2Pl (mondotok)",
      str_detect(name, 'form_3pl') ~  "Prs.NDef.3Pl (mondanak)" 
    ),
    type = case_when(
      str_detect(name, '_1$') ~ 'nv_form',
      str_detect(name, '_2$') ~ 'v_form'
    )
  ) |> 
  select(-name) |> 
  pivot_wider(names_from = type, values_from = value)
}

# take raw data, return responses with info on participant
gatherData = function(dat){
  ## meta
  
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
  
  survey_text = dat |> 
    filter(trial_type == 'survey-text') |> 
    pull(response)
  
  raw_id = str_extract(survey_text, '(?<="Q0":")[^"]+')
  gender = str_extract(survey_text, '(?<="Q1":")[^"]+')
  yob = str_extract(survey_text, '(?<="Q2":")[^"]+')
  
  check_answers = dat |> 
    filter(
      str_detect(stimulus, '^(Ki|Mi|Hány|Milyen|Hol)')
    ) |> 
    select(stimulus,response_string)
  
  got_right = left_join(correct_answers,check_answers, by = join_by(stimulus)) |> 
    mutate(correct = response_string == correct_response) |> 
    count(correct) |> 
    filter(correct) |> 
    pull(n)
  
  checks_passed = glue('{got_right} / 8')
  
  ## resp
  
  resp = dat |> 
    filter(
      str_detect(stimulus, '^Mari')
    ) |> 
    mutate(
      lemma_orth = str_extract(stimulus, '(?<=szívesen )[^.]*(?=\\. )'),
      tag = case_when(
        str_detect(stimulus, 'Téged én') ~ 'Prs.1Sg›2 (mondalak)',
        str_detect(stimulus, 'Ti is') ~ 'Prs.NDef.2Pl (mondotok)',
        str_detect(stimulus, 'Ők is') ~ 'Prs.NDef.3Pl (mondanak)',
        str_detect(stimulus, 'Te is') ~ 'Prs.NDef.2Sg (mondas)'
      )
    ) |> 
    select(lemma_orth,tag,response_string,rt,trial_index,time_elapsed) |> 
    mutate(
      rt = as.double(rt),
      gender = gender,
      yob = yob,
      checks_passed = checks_passed,
      raw_id = raw_id,
      user_info = user_info,
      total_time = sum(rt)
    )
  
  return(resp)
}

# -- read -- #

s = read_tsv('dat/nonwords_final.tsv')
correct_answers = read_tsv('dat/correct_answers_to_checks.tsv')

# -- collect responses -- #

sl = s |> longer()

my_path = '~/Github/Pavlovia/mondasz/data/'
file_name = list.files(my_path) 

d = tibble(
  file_name = file_name
) |> 
  filter(str_detect(file_name, 'mondasz_mondasz_participant')) |> 
  mutate(
    raw = map(file_name, ~ read_csv(glue("{my_path}{.}"))),
    tidy = map(raw, gatherData)
  ) |> 
  select(file_name,tidy) |> 
  unnest(tidy) |> 
  left_join(sl) |> 
  mutate(
    resp_v = response_string == v_form,
    rt_upper = median(rt) + 3 * mad(rt)
  )

# -- print info -- #

n_passed = d |>
  distinct(raw_id,checks_passed) |> 
  filter(checks_passed %in% c('8 / 8', '7 / 8', '6 / 8')) |> 
  nrow()

n_total = length(unique(d$raw_id))

glue('> {n_passed} / {n_total} participants passed checks.')

ids = d |> 
  distinct(raw_id) |> 
  arrange(raw_id)

# -- write -- #

write_tsv(d, 'dat/exp_data_tidy.tsv.gz')
write_tsv(ids, 'dat/passed.tsv')
