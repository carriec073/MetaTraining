## compute metadp with hierarchical bayesian method
niter = 10000
nchains = 4
library(magrittr)
library(rjags)
library(ggmcmc)

# dots task, T2, attention video
nR_S1 = counts %>%
  filter(feedback == 'T2', video == 'attention') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts %>%
  filter(feedback == 'T2', video == 'attention') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

source(here::here('functions', 'Function_metad_group.R'))

Hmeta_att <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)
# traceplot(Hmeta_att) # use to check caterpillar

# dots task, T2, meta video
nR_S1 = counts %>%
  filter(feedback == 'T2', video == 'meta') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts %>%
  filter(feedback == 'T2', video == 'meta') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

# Load the function file to ensure it's available
source(here::here('functions', 'Function_metad_group.R'))

Hmeta_meta <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)

# mcmc values in df for plot posterior distributions
mcmc_att <- ggs(Hmeta_att) %>%
  mutate(group = 'dots_T2_att') %>%
  filter(Parameter == "mu_logMratio")
mcmc_meta <- ggs(Hmeta_meta) %>%
  mutate(group = 'dots_T2_meta') %>%
  filter(Parameter == "mu_logMratio")
mcmc_T2 = rbind(mcmc_att, mcmc_meta)

Hmeta_att_avg_T2 = summary(Hmeta_att)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)
Hmeta_meta_avg_T2 = summary(Hmeta_meta)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)

