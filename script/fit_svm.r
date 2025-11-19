# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(broom)
library(kernlab)

# -- fun -- #

# fit svm on always the same data, but w/ diff hyperparameters
trainSVM = function(sigma, C, epsilon, train_matrix, test_matrix){
  
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
  
  # Get support vector indices
  sv_indices = SVindex(svm_model)
  # length(sv_indices)
  
  # Create kernel matrix: test points × support vectors only
  # test_kernel is currently 80 × 161 (test × all training)
  # We need: 80 × n_sv (test × support vectors)
  test_kernel_sv = test_kernel[, sv_indices]
  
  # Predict
  nonword_predictions = predict(svm_model, as.kernelMatrix(test_kernel_sv))
  
  return(nonword_predictions)
}

# get accuracy in tuning tibble
getAccuracy = function(nonword_predictions, d_lemma){
  d_lemma$pred = as.double(nonword_predictions)
  with(d_lemma, tidy(cor.test(lo_v,pred, method = 'kendall')))
}

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
  filter(lo_v > -3, lo_v < 3) |> 
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

# -- tune -- #

tuning = crossing(
  my_sigma = c(0.2, 0.5, 1),      # 3 values
  my_C = c(0.1, 1, 10),           # 3 values  
  my_epsilon = c(0.1, 0.2, 1)        # 2 values
)

tuned = tuning |> 
  mutate(
    pred = pmap(list(my_sigma, my_C, my_epsilon), 
               ~ trainSVM(sigma = ..1, 
                          C = ..2, 
                          epsilon = ..3, 
                          train_matrix = train_matrix, 
                          test_matrix = test_matrix)),
    acc = map(pred, ~ getAccuracy(., d_lemma))
  )

tuned |> 
  select(-pred) |> 
  unnest(acc) |> 
  filter(p.value == min(p.value))


