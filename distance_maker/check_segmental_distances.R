setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(patchwork)

makeDistMDS = function(dat){
  dat2 = dat |>
    dplyr::filter(segment1 != ' ', segment2 != ' ') |> 
    select(segment1,segment2,dist)
  dat_matrix = dat2 |>
    tidyr::pivot_wider(names_from = segment2, values_from = dist) |> 
    dplyr::select(-segment1) |> 
    as.matrix()
  dat_mds = stats::cmdscale(dat_matrix, k = 2)
  tidyr::tibble(
    x = dat_mds[,1],
    y = dat_mds[,2],
    label = unique(dat2$segment1)
  ) 
}

d1 = read_tsv('distance_maker/siptar_torkenczy_toth_racz_hungarian_dt.tsv')
d2 = read_tsv('distance_maker/segment_similarity_hungarian.tsv')

d2$dist = 1-d2$similarity

makeDistMDS(d1) |> 
  ggplot(aes(x,y,label = label)) +
  geom_label() +
  theme_bw()

makeDistMDS(d2) |> 
  ggplot(aes(x,y,label = label)) +
  geom_label() +
  theme_bw()

# ho hum