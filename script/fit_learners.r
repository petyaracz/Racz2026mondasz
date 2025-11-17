# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(broom)
library(kernlab)

# -- fun -- #

# ... lift music 

# -- read -- #

# distances, info on existing words, results on nonwords
t = read_tsv('distance_maker/aligned_word_pairs_phonological_distance.tsv')
c = read_tsv('dat/mondasz_training.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')

# -- setup -- #

dist = t |> 
  select(word1,word2,phon_dist)

# need cross-ref!
dist2 = tibble(
  word1 = dist$word2,
  word2 = dist$word1,
  phon_dist = dist$phon_dist
)

dist = bind_rows(dist,dist2)

corpus_odds = c |> 
  summarise(
    freq_v = sum(freq_v),
    freq_nv = sum(freq_nv),
    .by = lemma
  ) |> 
  mutate(lo_v = log(freq_v/freq_nv)) |> 
  arrange(lemma)

d_lemma = d |> 
  summarise(
    lo_v = qlogis(mean(resp_v)),
    .by = lemma
  ) |> 
  arrange(lemma)

# Get ordered word list
word_order = corpus_odds$lemma

# dist between real words only
train_dist = dist |> 
  filter(word1 %in% word_order, word2 %in% word_order)

# dist between nonwords and real words
test_dist = dist |> 
  filter(word1 %in% d_lemma$lemma, word2 %in% word_order)

# Convert to matrices WITH EXPLICIT ORDERING
train_matrix = train_dist |> 
  pivot_wider(names_from = word2, values_from = phon_dist) |>
  arrange(factor(word1, levels = word_order)) |>  # ← CRITICAL: force row order
  select(word1, all_of(word_order)) |>            # ← CRITICAL: force column order
  select(-word1) |> 
  as.matrix()

test_matrix = test_dist |> 
  pivot_wider(names_from = word2, values_from = phon_dist) |> 
  arrange(word1) |>  # sort test words (order doesn't matter for test)
  select(word1, all_of(word_order)) |>  # ← CRITICAL: columns match train_matrix
  select(-word1) |> 
  as.matrix()

# kill nas on diagonal

train_matrix[is.na(train_matrix)] = 0
test_matrix[is.na(test_matrix)] = 0

# Verify dimensions and ordering
stopifnot(ncol(train_matrix) == nrow(corpus_odds))
stopifnot(nrow(train_matrix) == nrow(corpus_odds))
stopifnot(ncol(test_matrix) == nrow(corpus_odds))

# Convert to kernel matrices
sigma = 1 # tune later
C = .1
epsilon = .5

train_kernel = exp(-train_matrix^2 / (2 * sigma^2))
test_kernel = exp(-test_matrix^2 / (2 * sigma^2))

# Fit model
svm_model = ksvm(x = as.kernelMatrix(train_kernel),
                 y = corpus_odds$lo_v,
                 kernel = "matrix",
                 type = "eps-svr",
                 C = C,
                 epsilon = epsilon
                 )

# Predict

## training

corpus_odds$pred = predict(svm_model)

corpus_odds |> 
  ggplot(aes(lo_v,pred)) +
  geom_point()

## test

# Get support vector indices
sv_indices = SVindex(svm_model)
length(sv_indices)

# Create kernel matrix: test points × support vectors only
# test_kernel is currently 80 × 161 (test × all training)
# We need: 80 × n_sv (test × support vectors)
test_kernel_sv = test_kernel[, sv_indices]

# Predict
nonword_predictions = predict(svm_model, as.kernelMatrix(test_kernel_sv))

d_lemma$pred = nonword_predictions

d_lemma |> 
  ggplot(aes(lo_v,pred,label = lemma)) +
  geom_label()
