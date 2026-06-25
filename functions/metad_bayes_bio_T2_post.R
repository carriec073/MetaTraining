# binarise confidence
df_post$binconf <- ifelse(df_post$ConfResp > 3, 1, 0)

# count trials per condition
counts_post <- df_post |>
  group_by(task, video, feedback, ID, click, binconf, stim) |>
  summarise(ntrials = n(), .groups = "drop") |>
  complete(
    nesting(task, video, feedback, ID),
    click = unique(df_post$click),
    binconf = unique(df_post$binconf),
    stim = unique(df_post$stim),
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

# bio task, T2, attention video
nR_S1 = counts_post %>%
  filter(feedback == 'T2', video == 'attention',task == 'bio') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_post %>%
  filter(feedback == 'T2', video == 'attention',task == 'bio') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

source(here::here('functions', 'Function_metad_group.R'))

Hmeta_att_bio_T2_post <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)
# traceplot(Hmeta_att) # use to check caterpillar

####__ High Type 2 (Metacognitive Instruction)

# bio task, T2, meta video
nR_S1 = counts_post %>%
  filter(feedback == 'T2', video == 'meta',task == 'bio') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_post %>%
  filter(feedback == 'T2', video == 'meta',task == 'bio') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

# Load the function file to ensure it's available
source(here::here('functions', 'Function_metad_group.R'))

Hmeta_meta_bio_T2_post <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)

# mcmc values in df for plot posterior distributions
mcmc_att_bio_T2_post <- ggs(Hmeta_att_bio_T2_post) %>%
  mutate(group = 'bio_T2_att_post') %>%
  filter(Parameter == "mu_logMratio")
mcmc_meta_bio_T2_post <- ggs(Hmeta_meta_bio_T2_post) %>%
  mutate(group = 'bio_T2_meta_post') %>%
  filter(Parameter == "mu_logMratio")
mcmc_bio_T2_post = rbind(mcmc_att_bio_T2_post, mcmc_meta_bio_T2_post)

Hmeta_att_avg_bio_T2_post = summary(Hmeta_att_bio_T2_post)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)
Hmeta_meta_avg_bio_T2_post = summary(Hmeta_meta_bio_T2_post)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)

# Save as CSV
write.csv(mcmc_att_bio_T2_post, file = here::here("outputs", "mcmc_results_att_bio_T2_post.csv"), row.names = FALSE)
write.csv(mcmc_meta_bio_T2_post, file = here::here("outputs", "mcmc_results_meta_bio_T2_post.csv"), row.names = FALSE)

# Save summary statistics
mcmc_att_T2_bio_post_summary <- summary(mcmc_att_bio_T2_post)
mcmc_meta_T2_bio_post_summary <- summary(mcmc_meta_bio_T2_post)

# Print completion message
cat("bio task T2 MCMC analysis completed and saved to:", here::here("outputs"), "\n")

