################################################
# plot distances
################################################

# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)

# -- fun -- #


# take forms after filt, return distance matrix and coordinates.
getDist = function(forms, coord_only = T){
  
  ## get lemmata
  
  d2 = forms |> 
    distinct(lemma_orth,form,category)
  
  ## make all pairs
  
  dist = crossing(
    form1 = d2$form,
    form2 = d2$form
  ) # try to keep nrow dist in some normal range, like under 10^6
  
  ## build matrix 
  
  dat_matrix_edit = dist |>
    mutate(dist = stringdist::stringdist(form1,form2, method = 'lv')) |> 
    pivot_wider(names_from = form2, values_from = dist) |>
    select(-form1) |> 
    as.matrix()
  
  ## get dims
  
  mds_edit = stats::cmdscale(dat_matrix_edit, k = 2)
  
  mds_table = tibble(
    form = unique(dist$form1), #!
    x = mds_edit[,1],
    y = mds_edit[,2]
  ) |> 
    left_join(d2)
  
  ifelse(
    coord_only,
    return(mds_table),
    return(list(dat_matrix_edit,mds_table))  
  )
  
}

# -- read -- #

d = read_tsv('dat/mondasz_mondsz_webcorpus.tsv')

# -- setup -- #

forms = d |> 
  filter(
    !is.na(category),
    (varies & class == 'cc') | (freq_v > 9 | freq_nv > 9)
  ) |> 
  distinct(lemma,lemma_orth,category,form,tag)

# add basic form
lemmata = d |> 
  distinct(lemma,lemma_orth,category) |> 
  mutate(form = lemma) |> 
  mutate(tag = 'Prs.NDef.3Sg (mond)')

all_forms = bind_rows(forms,lemmata)

# -- dist -- #

forms_nested = all_forms |> 
  nest(.by = tag)

coords = forms_nested |> 
  mutate(
    coord = map(data, ~ getDist(., coord_only = T))
  ) |> 
  select(tag,coord) |> 
  unnest(coord)

# -- write -- #

write_tsv(coords, 'dat/coordinates.gz')

# -- plot -- #
# 
# library(ggforce)
# 
# coord |> 
#   mutate(category = fct_relevel(category, 'test')) |> 
#   ggplot(aes(x, y, colour = category, fill = category)) +
#   ggforce::geom_mark_hull(alpha = 0.2, colour = NA, concavity = 3) +
#   geom_point(alpha = 0.3, size = 0.5) + 
#   theme_void() +
#   scale_fill_colorblind() +
#   scale_colour_colorblind() +
#   theme(legend.position = 'bottom')
# 
