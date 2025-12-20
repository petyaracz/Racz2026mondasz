# -- head -- #

setwd('~/Github/Racz2026mondasz/')

library(tidyverse)
library(ggthemes)
library(lme4)
library(performance)
library(sjPlot)

# -- read -- #

d = read_tsv('dat/krr_results.tsv')

# -- tidy -- #

d = d |> 
  filter(rt < rt_upper)

# -- aggr -- #

items = d |> 
  count(resp_v,lemma_orth,krr_pred,tag,coda) |> 
  pivot_wider(names_from = resp_v, values_from = n, values_fill = 0) |> 
  rename(resp_v = `TRUE`, resp_nv = `FALSE`) |> 
  mutate(p = resp_v / (resp_v + resp_nv))

parts = d |> 
  count(resp_v,raw_id,tag,coda) |> 
  pivot_wider(names_from = resp_v, values_from = n, values_fill = 0) |> 
  rename(resp_v = `TRUE`, resp_nv = `FALSE`) |> 
  mutate(p = resp_v / (resp_v + resp_nv))

# -- eyeball -- #

items |> 
  ggplot(aes(krr_pred,p,colour = coda)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ tag) +
  scale_colour_colorblind()

items |> 
  ggplot(aes(krr_pred,p,colour = tag)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ coda) +
  scale_colour_colorblind()

parts |> 
  ggplot(aes(p,coda,colour = tag)) +
  geom_boxplot() +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind()

parts |> 
  ggplot(aes(p,tag,colour = coda)) +
  geom_boxplot() +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind()

# -- fit -- #

fit0 = glmer(as.double(resp_v) ~ krr_pred + (1|raw_id), data = d, family = binomial)
fit1 = glmer(resp_v ~ krr_pred + coda + tag + (1|raw_id), data = d, family = binomial)
fit2 = glmer(resp_v ~ coda + tag + (1|raw_id), data = d, family = binomial)
fit3 = glmer(as.double(resp_v) ~ coda * tag + (1|raw_id), data = d, family = binomial)
fit4 = glmer(resp_v ~ krr_pred + tag + (1|raw_id), data = d, family = binomial)
fit5 = glmer(resp_v ~ krr_pred * coda + tag + (1|raw_id), data = d, family = binomial)
fit6 = glmer(resp_v ~ krr_pred * tag + coda + (1|raw_id), data = d, family = binomial)
fit7 = glmer(resp_v ~ krr_pred + coda * tag + (1|raw_id), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
fit8 = glmer(resp_v ~ krr_pred * coda * tag + (1|raw_id), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))

# -- eval -- #

plot(compare_performance(fit0,fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8))
# fit3 by a mile

# -- check -- #

summary(fit3)
plot_model(fit0, 'pred', terms = 'krr_pred')
plot_model(fit3, 'pred', terms = c('coda','tag'))
plot_model(fit3, 'pred', terms = c('tag','coda'))

# -- resid -- #

d$resid_coda_tag = resid(fit3)

fit9 = lmer(resid_coda_tag ~ krr_pred + (1|raw_id), data = d)
summary(fit9)

plot_model(fit9, 'pred', terms = 'krr_pred')
# mondjuk ez mar eleg sulyos