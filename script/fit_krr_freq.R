setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(rstanarm)
library(performance)

# Kernel ridge regression function
trainKRR = function(sigma, alpha, train_matrix, test_matrix, target){
  
  train_kernel = exp(-train_matrix^2 / (2 * sigma^2))
  test_kernel = exp(-test_matrix^2 / (2 * sigma^2))
  
  # Fit model: (K + αI)^-1 * y
  n = nrow(train_kernel)
  K_reg = train_kernel + alpha * diag(n)
  coefficients = solve(K_reg, target)
  
  # Predict: K_test * coefficients
  nonword_predictions = test_kernel %*% coefficients
  
  return(as.vector(nonword_predictions))
}

# tune model
tuneModel = function(my_sigma,my_alpha){
  
  # fit KRR
  predictions = trainKRR(
    sigma = my_sigma,          
    alpha = my_alpha,         
    train_matrix = train_matrix,
    test_matrix = test_matrix,
    target = real_words$llfpm10
  )
  
  # get results and put them back in d
  results = tibble(
    lemma = nonwords,
    pred = predictions
  ) |> 
    left_join(dsum, by = join_by(lemma))
  
  r = with(results, cor(p,pred, method = 'pearson'))
  
  return(r)  
}

# -- read -- #

print('Loading data...')

c = read_tsv('dat/mondasz_training.tsv')
d = read_tsv('dat/exp_data_tidy.tsv.gz')  # lemma, p
dist = read_tsv('dat/aligned_word_pairs_phonological_distance.tsv.gz')  # word1, word2, distance

d = d |> filter(
  checks_passed != '5 / 8',
  rt < rt_upper
)

# -- pin down outcome var -- #

csum = c |> 
  distinct(lemma,lemma_orth,llfpm10)

dsum = d |> 
  summarise(
    p = mean(resp_v),
    .by = c(lemma,lemma_orth)
  )

# -- setup -- #

print('Setting up distance matrix...')

# Separate real words from nonwords
real_words = csum |>
  arrange(lemma)

nonwords = dsum |> 
  arrange(lemma) |> 
  pull(lemma)

# Get ordered word list for training
word_order = real_words$lemma

# Distance matrices
# Real words × real words
train_dist = dist |> 
  filter(word1 %in% word_order, word2 %in% word_order)

# Nonwords × real words
test_dist = dist |> 
  filter(word1 %in% nonwords, word2 %in% word_order)

# Convert to matrices WITH EXPLICIT ORDERING
train_matrix = train_dist |> 
  pivot_wider(names_from = word2, values_from = phon_dist) |>  # check your distance column name!
  arrange(factor(word1, levels = word_order)) |>
  select(word1, all_of(word_order)) |>
  select(-word1) |> 
  as.matrix()

test_matrix = test_dist |> 
  pivot_wider(names_from = word2, values_from = phon_dist) |>
  arrange(word1) |>
  select(word1, all_of(word_order)) |>
  select(-word1) |> 
  as.matrix()

# -- tune model -- #

print('Tuning model... (stay tuned, LOL)')

tuning = crossing(
  my_sigma = c(1, 2, 3, 8, 10),
  my_alpha = c(0.01, 0.1, 1, 10, 100)
)

tictoc::tic('tuning duration')

tuned = tuning |> 
  mutate(
    r = map2_dbl(my_sigma, my_alpha, ~ tuneModel(.x,.y))
  )

tictoc::toc()

print('Fitting best model...')

tuned_max = filter(tuned, r == max(r))

# test
predictions = trainKRR(
  sigma = tuned_max$my_sigma,           # adjust based on your distance scale
  alpha = tuned_max$my_alpha,         # adjust based on overfitting
  train_matrix = train_matrix,
  test_matrix = test_matrix,
  target = real_words$llfpm10 # !!!
)

# Results
results = tibble(
  lemma = nonwords,
  krr_pred = predictions
) |> 
  left_join(d)

results$alpha = tuned_max$my_alpha
results$sigma = tuned_max$my_sigma

print('Writing to file...')

results |> 
  write_tsv('dat/krr_results_experiment_freq.tsv')

