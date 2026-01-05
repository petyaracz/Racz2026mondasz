# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)
library(lme4)
library(performance)
library(sjPlot)
library(patchwork)

# -- read -- #

c = read_tsv('dat/krr_results_corpus.tsv')
d = read_tsv('dat/krr_results_experiment.tsv')

# -- tidy -- #

d = d |> 
  mutate(krr_scaled = scales::rescale(krr_pred))

c = c |> 
  mutate(krr_scaled = scales::rescale(krr_pred))

# -- fit -- #

fit0 = glmer(resp_v ~ krr_scaled + coda + tag + (1|raw_id) + (1|lemma), data = d, family = binomial)
fit1 = glmer(as.double(resp_v) ~ krr_scaled + (1|raw_id) + (1|lemma), data = d, family = binomial)
fit2 = glmer(as.double(resp_v) ~ coda + tag + (1|raw_id) + (1|lemma), data = d, family = binomial)
fit3 = glmer(resp_v ~ coda * tag + (1|raw_id) + (1|lemma), data = d, family = binomial)
fit4 = glmer(resp_v ~ krr_scaled * coda + tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit5 = glmer(as.double(resp_v) ~ krr_scaled * tag + coda + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit6 = glmer(as.double(resp_v) ~ krr_scaled + coda * tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit7 = glmer(resp_v ~ krr_scaled * coda * tag + (1|raw_id) + (1|lemma), data = d, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun = 100000)))

# -- eval -- #

check_collinearity(fit3) # out
check_collinearity(fit4) # out
check_collinearity(fit5) # out

plot(compare_performance(fit0,fit1,fit2)) +
  scale_colour_viridis_d(option = 'H')

# -- resid -- #

d$resid = resid(fit2)

fit11 = lmer(resid ~ krr_scaled + (1|raw_id) + (1|lemma), data = d)

summary(fit11)

# -- viz -- #

plot_model(fit2, 'pred', terms = c('tag','coda')) +
  theme_few() +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  coord_flip()
plot_model(fit11, 'pred', terms = 'krr_scaled') +
  theme_few()

# -- subsetting -- #

fits = d |> 
  nest(
    .by = c(tag,coda)
  ) |> 
  mutate(
    tagcoda = glue::glue('{tag} {coda}'),
    fit = map(data,
              ~ glmer(as.double(resp_v) ~ krr_scaled + (1|raw_id), data = ., family = binomial)
              ),
    tidy = map(fit, 
               ~ broom.mixed::tidy(., conf.int = T)
               ),
    plot = map2(fit, tagcoda, 
               ~ plot_model(.x, 'pred', terms = 'krr_scaled') + theme_few() + ggtitle(.y) + xlab('') + ylab('')
                 )
  )

wrap_plots(fits$plot)

fits2 = d |> 
  nest(
    .by = tag
  ) |> 
  mutate(
    fit = map(data,
              ~ glmer(as.double(resp_v) ~ krr_scaled + (1|raw_id), data = ., family = binomial)
    ),
    tidy = map(fit, 
               ~ broom.mixed::tidy(., conf.int = T)
    ),
    plot = map2(fit, tag, 
                ~ plot_model(.x, 'pred', terms = 'krr_scaled') + theme_few() + ggtitle(.y) + xlab('') + ylab('')
    )
  )

wrap_plots(fits2$plot)

fits3 = d |> 
  nest(
    .by = coda
  ) |> 
  mutate(
    fit = map(data,
              ~ glmer(as.double(resp_v) ~ krr_scaled + (1|raw_id), data = ., family = binomial)
    ),
    tidy = map(fit, 
               ~ broom.mixed::tidy(., conf.int = T)
    ),
    plot = map2(fit, coda, 
                ~ plot_model(.x, 'pred', terms = 'krr_scaled') + theme_few() + ggtitle(.y) + xlab('') + ylab('')
    )
  )

wrap_plots(fits3$plot)
summary(fit2)

# -- rt -- #

d$log_rt = log(d$rt)

fit21 = lmer(rt ~ resp_v + coda + tag + (1|raw_id) + (1|lemma), data = d)

summary(fit21)
