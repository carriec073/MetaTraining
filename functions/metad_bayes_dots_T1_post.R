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

# dots task, T1, attention video
nR_S1 = counts_post %>%
  filter(feedback == 'T1', video == 'attention', task == 'dots') %>%
  select(ID, left) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, left) %>%
  select(-cat) %>%
  as.data.frame()
nR_S1[is.na(nR_S1)] = 0

nR_S2 = counts_post %>%
  filter(feedback == 'T1', video == 'attention', task == 'dots') %>%
  select(ID, right) %>%
  group_by(ID) %>%
  dplyr::mutate(cat = 1:n()) %>%
  spread(ID, right) %>%
  select(-cat) %>%
  as.data.frame()
nR_S2[is.na(nR_S2)] = 0

source(here::here('functions', 'Function_metad_group.R'))

#need to change this and below for each treatment
Hmeta_att_dots_T1_post <- metad_group(
  list(nR_S1),
  list(nR_S2),
  niter,
  nchains,
  here::here('functions')
)
# traceplot(Hmeta_att) # use to check caterpillar


# mcmc values in df for plot posterior distributions
mcmc_att_dots_T1_post <- ggs(Hmeta_att_dots_T1_post) %>%
  mutate(group = 'dots_T1_att_post') %>%
  filter(Parameter == "mu_logMratio")

Hmeta_att_avg_dots_T1_post = summary(Hmeta_att_dots_T1_post)$statistics[, 'Mean'] %>%
  as.data.frame() %>%
  head(-3)

# Save as CSV
write.csv(mcmc_att_dots_T1_post, file = here::here("outputs", "mcmc_results_dots_T1_post.csv"), row.names = FALSE)

# Save summary statistics
mcmc_att_dots_T1_post_summary <- summary(mcmc_att_dots_T1_post)
#saveRDS(mcmc_att_dots_T1_summary, file = here::here("functions", "output", "mcmc_att_dots_T1_summary.rds"))

# Print completion message
cat("dots task T1 MCMC analysis completed and saved to:", here::here("functions", "output"), "\n")

