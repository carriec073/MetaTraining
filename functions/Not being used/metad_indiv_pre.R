# binarise confidence
df_pre$binconf <- ifelse(df_pre$ConfResp > 3, 1, 0)

# count trials per condition
counts <- df_pre |>
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

# re-order stuff so it fits the desired format
#counts$right[!is.na(counts$right2)] <- counts$right2[!is.na(counts$right2)]
#counts$left[!is.na(counts$left2)] <- counts$left2[!is.na(counts$left2)]

# fit metad; mle generates outliers, prefer sse
metaperf <- counts |>
  group_by(task, video, feedback, ID) |>
  # partition(cluster)|>
  do(fit_meta_d_SSE(.$left, .$right))
  # collect()

# select relevant columns only
metaperf <- metaperf |>
  mutate(niter = 1:n()) |>
  filter(niter == 1) |>
  select(task, video, feedback, ID, da, meta_da, M_ratio)

# estimate type 2 ROC curve
# rocperf <- a|>
#   group_by(group, suj)|>
#   do(computeROC(.$quantconf, .$cor, bins))
# indivdata <- full_join(indivdata, metaperf_mle)
# indivdata <- full_join(indivdata, rocperf)
