
# take specific word, s, p, return word weight
fitGCM = function(dat, var_s, var_p){
  
  my_weight = dat |> 
    mutate(
      pairwise_sim = exp ( - dist / var_s )^var_p,
      total_sim = sum(pairwise_sim)
    ) |> 
    group_by(category) |> 
    mutate(
      category_sim = sum(pairwise_sim),
    ) |> 
    ungroup() |> 
    mutate(
      weight = category_sim / total_sim
    ) |> 
    filter(category == 'vc training') |> 
    distinct(
      weight
    )
  
  return(my_weight)
}

# map fitGCM through an entire word list, return words w/ weights
categoryGCM = function(dat, my_s, my_p){
  dat |>
    nest(.by = test) |> # test, not form
    mutate(
      weight = map(data, ~fitGCM(., var_s = my_s, var_p = my_p))
    ) |>
    select(test, weight) |>
    unnest(
      weight
    )  
}

# take output of categoryGCM, merge with d, return r2 based on deviance (best metric, since glm optimises for it and the glms only differ in what the predictor is, not complexity)
evalGCM = function(d,dat,is_mond = F){
  if (is_mond){
    dat2 = rename(dat, lemma = test)
  } else {
    dat2 = dat
  }
  dat2 = inner_join(d,dat2)
  fit1 = glm(cbind(freq_nv,freq_v) ~ weight, data = dat2, family = binomial)  
  fit0 = glm(cbind(freq_nv,freq_v) ~ 1, data = dat2, family = binomial)
  r2 = 1 - deviance(fit1) / deviance(fit0)
  return(r2)
}
