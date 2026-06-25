# binarise confidence
df_pre$binconf <- ifelse(df_pre$ConfResp > 3, 1, 0)

# count trials per condition
counts_pre <- df_pre |>
  group_by(task, video, feedback, ID, click, binconf, stim) |>
  summarise(ntrials = n(), .groups = "drop") |>
  complete(
    nesting(task, video, feedback, ID),
    click = unique(df_pre$click),
    binconf = unique(df_pre$binconf),
    stim = unique(df_pre$stim),
    fill = list(ntrials = 0)
  ) |>
  spread(stim, ntrials) |>
  ungroup()

## compute metadp with hierarchical bayesian method
niter = 10000
nchains = 4
library(magrittr)
library(rjags)
library(ggmcmc)

# dots task, T2, attention video
nR_S1 = counts_pre %>%
  filter(feedback == 'T2', video == 'attention',task == 'dots') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_pre %>%
  filter(feedback == 'T2', video == 'attention',task == 'dots') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

source(here::here('functions', 'Function_metad_group.R'))

Hmeta_att_dots_T2_pre <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)
# traceplot(Hmeta_att) # use to check caterpillar

####__ High Type 2 (Metacognitive Instruction)

# dots task, T2, meta video
nR_S1 = counts_pre %>%
  filter(feedback == 'T2', video == 'meta',task == 'dots') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_pre %>%
  filter(feedback == 'T2', video == 'meta',task == 'dots') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

# Load the function file to ensure it's available
source(here::here('functions', 'Function_metad_group.R'))

Hmeta_meta_dots_T2_pre <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)

# mcmc values in df for plot posterior distributions
mcmc_att_dots_T2_pre <- ggs(Hmeta_att_dots_T2_pre) %>%
  mutate(group = 'dots_T2_att_pre') %>%
  filter(Parameter == "mu_logMratio")
mcmc_meta_dots_T2_pre <- ggs(Hmeta_meta_dots_T2_pre) %>%
  mutate(group = 'dots_T2_meta_pre') %>%
  filter(Parameter == "mu_logMratio")
mcmc_dots_T2_pre = rbind(mcmc_att_dots_T2_pre, mcmc_meta_dots_T2_pre)

Hmeta_att_avg_dots_T2_pre = summary(Hmeta_att_dots_T2_pre)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)
Hmeta_meta_avg_dots_T2_pre = summary(Hmeta_meta_dots_T2_pre)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)

# Save as CSV
write.csv(mcmc_att_dots_T2_pre, file = here::here("outputs", "mcmc_results_att_dots_T2_pre.csv"), row.names = FALSE)
write.csv(mcmc_meta_dots_T2_pre, file = here::here("outputs", "mcmc_results_meta_dots_T2_pre.csv"), row.names = FALSE)

# Save summary statistics
mcmc_att_T2_dots_pre_summary <- summary(mcmc_att_dots_T2_pre)
mcmc_meta_T2_dots_pre_summary <- summary(mcmc_meta_dots_T2_pre)

# Print completion message
cat("dots task T2 MCMC analysis completed and saved to:", here::here("outputs"), "\n")

