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

# dots task, T1, attention video
nR_S1 = counts_pre %>%
  filter(feedback == 'none', video == 'attention', task == 'dots') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_pre %>%
  filter(feedback == 'none', video == 'attention', task == 'dots') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

source(here::here('functions', 'Function_metad_group.R'))

#need to change this and below for each treatment
Hmeta_att_dots_none_pre <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)
# traceplot(Hmeta_att) # use to check caterpillar


# mcmc values in df for plot posterior distributions
mcmc_att_dots_none_pre <- ggs(Hmeta_att_dots_none_pre) %>%
  mutate(group = 'dots_none_att_pre') %>%
  filter(Parameter == "mu_logMratio")

Hmeta_att_avg_dots_none_pre = summary(Hmeta_att_dots_none_pre)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)

# Save as CSV
write.csv(mcmc_att_dots_none_pre, file = here::here("outputs", "mcmc_results_dots_none_pre.csv"), row.names = FALSE)

# Save summary statistics
mcmc_att_dots_none_pre_summary <- summary(mcmc_att_dots_none_pre)
#saveRDS(mcmc_att_dots_none_summary, file = here::here("functions", "output", "mcmc_att_dots_none_summary.rds"))

# Print completion message
cat("Bio task None MCMC analysis completed and saved to:", here::here("functions", "output"), "\n")

