# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(stringdist)
library(glue)

# -- read -- #

d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- subs -- #

t = d |> 
  filter( 
    category == 'test',
    # varies
  )

# -- count -- #

t |>
  distinct(form,coda) |> 
  count(coda, sort = T)

t |> 
  distinct(form,coda,lemma) |> 
  count(lemma,coda, sort = T)

t |> 
  filter(coda %in% c('ng')) |> 
  distinct(lemma) |> 
  pull(lemma)

t |> 
  count(coda,tag, sort = T) |> 
  pivot_wider(names_from = 'tag', values_from = 'n', values_fill = 0) |> View()

t2 = t |> 
  filter(c1 == 'n' | c2 %in% c('t','d'))

# -- viz -- #

t2 |> count(coda)

t2 |> 
  ggplot(aes(coda)) +
  geom_bar()

t2 |> 
  ggplot(aes(lo_v)) +
  geom_histogram() +
  facet_wrap( ~ coda)

t2 |> 
  ggplot(aes(lo_v)) +
  geom_histogram()

t2 |> 
  mutate(form = fct_reorder(form, lo_v)) |> 
  ggplot(aes(lo_v,form)) +
  geom_point() +
  geom_vline(xintercept = c(-3,3)) +
  facet_wrap( ~ tag, nrow = 1) 

t2 |> 
  filter(lo_v > -3, lo_v < 3) |> 
  mutate(form = fct_reorder(form, lo_v)) |> 
  ggplot(aes(lo_v,form)) +
  geom_point() +
  facet_wrap( ~ tag, nrow = 1) 

t2 |> 
  filter(str_detect(tag, 'dalak|dotok|das|danak')) |> 
  mutate(form = fct_reorder(form, lo_v)) |> 
  ggplot(aes(form,lo_v)) +
  geom_hline(yintercept = c(-3,3)) +
  geom_point() +
  facet_wrap( ~ tag, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

t2 |> 
  filter(str_detect(tag, 'dalak|dotok|das|danak'), lo_v > -3, lo_v < 3) |> 
  mutate(form = fct_reorder(form, lo_v)) |> 
  ggplot(aes(form,lo_v)) +
  geom_point() +
  facet_wrap( ~ tag, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


t3 = t2 |> 
  filter(str_detect(tag, 'dalak|dotok|das|danak'),lo_v > -5, lo_v < 5)
  

t3 |> 
  select(lemma,tag,lo_v) |> 
  pivot_wider(names_from = tag, values_from = lo_v) |> View()

t3 |> 
  select(lemma,tag,lo_v) |> 
  mutate(lemma_count = n(), .by = lemma) |> 
  filter(lemma_count > 1) |> 
  ggplot(aes(x = reorder(lemma, lo_v), y = lo_v, colour = tag, group = lemma)) +
  geom_line(colour = "grey50", linewidth = 0.5) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
