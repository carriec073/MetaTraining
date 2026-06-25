# Load necessary libraries
library(tidyverse)
library(HDInterval)
library(here)
library(cowplot)
library(bayestestR)

# Load the MCMC results - only necessary if you haven't previously loaded them
mcmc_bio_none_pre <- read_csv(file = here("outputs", "mcmc_results_bio_none_pre.csv"))
mcmc_bio_none_post <- read_csv(file = here("outputs", "mcmc_results_bio_none_post.csv"))
mcmc_bio_T1_pre <- read_csv(file = here("outputs", "mcmc_results_bio_T1_pre.csv"))
mcmc_bio_T1_post <- read_csv(file = here("outputs", "mcmc_results_bio_T1_post.csv"))
mcmc_meta_bio_T2_pre <- read_csv(file = here("outputs", "mcmc_results_meta_bio_T2_pre.csv"))
mcmc_meta_bio_T2_post <- read_csv(file = here("outputs", "mcmc_results_meta_bio_T2_post.csv"))
mcmc_att_bio_T2_pre <- read_csv(file = here("outputs", "mcmc_results_att_bio_T2_pre.csv"))
mcmc_att_bio_T2_post <- read_csv(file = here("outputs", "mcmc_results_att_bio_T2_post.csv"))
mcmc_dots_none_pre <- read_csv(file = here("outputs", "mcmc_results_dots_none_pre.csv"))
mcmc_dots_none_post <- read_csv(file = here("outputs", "mcmc_results_dots_none_post.csv"))
mcmc_dots_T1_pre <- read_csv(file = here("outputs", "mcmc_results_dots_T1_pre.csv"))
mcmc_dots_T1_post <- read_csv(file = here("outputs", "mcmc_results_dots_T1_post.csv"))
mcmc_meta_dots_T2_pre <- read_csv(file = here("outputs", "mcmc_results_meta_dots_T2_pre.csv"))
mcmc_meta_dots_T2_post <- read_csv(file = here("outputs", "mcmc_results_meta_dots_T2_post.csv"))
mcmc_att_dots_T2_pre <- read_csv(file = here("outputs", "mcmc_results_att_dots_T2_pre.csv"))
mcmc_att_dots_T2_post <- read_csv(file = here("outputs", "mcmc_results_att_dots_T2_post.csv"))


# Transform the data (exponentiate to get M-ratio)
mcmc_bio_none_pre <- mcmc_bio_none_pre %>% mutate(M_ratio = exp(value))
mcmc_bio_none_post <- mcmc_bio_none_post %>% mutate(M_ratio = exp(value))
mcmc_bio_T1_pre <- mcmc_bio_T1_pre %>% mutate(M_ratio = exp(value))
mcmc_bio_T1_post <- mcmc_bio_T1_post %>% mutate(M_ratio = exp(value))
mcmc_meta_bio_T2_pre <- mcmc_meta_bio_T2_pre %>% mutate(M_ratio = exp(value))
mcmc_meta_bio_T2_post <- mcmc_meta_bio_T2_post %>% mutate(M_ratio = exp(value))
mcmc_att_bio_T2_pre <- mcmc_att_bio_T2_pre %>% mutate(M_ratio = exp(value))
mcmc_att_bio_T2_post <- mcmc_att_bio_T2_post %>% mutate(M_ratio = exp(value))

mcmc_dots_none_pre <- mcmc_dots_none_pre %>% mutate(M_ratio = exp(value))
mcmc_dots_none_post <- mcmc_dots_none_post %>% mutate(M_ratio = exp(value))
mcmc_dots_T1_pre <- mcmc_dots_T1_pre %>% mutate(M_ratio = exp(value))
mcmc_dots_T1_post <- mcmc_dots_T1_post %>% mutate(M_ratio = exp(value))
mcmc_meta_dots_T2_pre <- mcmc_meta_dots_T2_pre %>% mutate(M_ratio = exp(value))
mcmc_meta_dots_T2_post <- mcmc_meta_dots_T2_post %>% mutate(M_ratio = exp(value))
mcmc_att_dots_T2_pre <- mcmc_att_dots_T2_pre %>% mutate(M_ratio = exp(value))
mcmc_att_dots_T2_post <- mcmc_att_dots_T2_post %>% mutate(M_ratio = exp(value))

#To view the distribution of mcmc values of M-ratio. Change the name of vector to explore other conditions
mcmc_bio_none_pre %>%
  ggplot(aes(M_ratio)) +
  geom_density(alpha = 0.4, position = "identity", color = "maroon") +
  labs(x = "M-ratio posterior estimate (Bio none Pre)", y = "Density") +
  xlim(-0.5,0.75)+
  guides(fill="none")

# Calculate 95% HDIs for both distributions
hdi_bio_none_pre <- hdi(mcmc_bio_none_pre$M_ratio, credMass = 0.95)
hdi_bio_none_post <- hdi(mcmc_bio_none_post$M_ratio, credMass = 0.95)
hdi_bio_T1_pre <- hdi(mcmc_bio_T1_pre$M_ratio, credMass = 0.95)
hdi_bio_T1_post <- hdi(mcmc_bio_T1_post$M_ratio, credMass = 0.95)
hdi_meta_bio_T2_pre <- hdi(mcmc_meta_bio_T2_pre$M_ratio, credMass = 0.95)
hdi_meta_bio_T2_post <- hdi(mcmc_meta_bio_T2_post$M_ratio, credMass = 0.95)
hdi_att_bio_T2_pre <- hdi(mcmc_att_bio_T2_pre$M_ratio, credMass = 0.95)
hdi_att_bio_T2_post <- hdi(mcmc_att_bio_T2_post$M_ratio, credMass = 0.95)

hdi_dots_none_pre <- hdi(mcmc_dots_none_pre$M_ratio, credMass = 0.95)
hdi_dots_none_post <- hdi(mcmc_dots_none_post$M_ratio, credMass = 0.95)
hdi_dots_T1_pre <- hdi(mcmc_dots_T1_pre$M_ratio, credMass = 0.95)
hdi_dots_T1_post <- hdi(mcmc_dots_T1_post$M_ratio, credMass = 0.95)
hdi_meta_dots_T2_pre <- hdi(mcmc_meta_dots_T2_pre$M_ratio, credMass = 0.95)
hdi_meta_dots_T2_post <- hdi(mcmc_meta_dots_T2_post$M_ratio, credMass = 0.95)
hdi_att_dots_T2_pre <- hdi(mcmc_att_dots_T2_pre$M_ratio, credMass = 0.95)
hdi_att_dots_T2_post <- hdi(mcmc_att_dots_T2_post$M_ratio, credMass = 0.95)

#### Summarize the high density intervals for conditions and timepoints and display median value ####
# --- BIO ---
cat("Bio No FB - Pre:  [", round(hdi_bio_none_pre$CI_low, 3),  ", ", round(hdi_bio_none_pre$CI_high, 3),  "]\n", median(mcmc_bio_none_pre$M_ratio), sep = "")
cat("Bio No FB - Post: [", round(hdi_bio_none_post$CI_low, 3), ", ", round(hdi_bio_none_post$CI_high, 3),  "]\n", median(mcmc_bio_none_post$M_ratio),sep = "")
cat("Bio Perf FB - Pre:  [", round(hdi_bio_T1_pre$CI_low, 3),    ", ", round(hdi_bio_T1_pre$CI_high, 3),    "]\n", median(mcmc_bio_T1_pre$M_ratio), sep = "")
cat("Bio Perf FB - Post: [", round(hdi_bio_T1_post$CI_low, 3),   ", ", round(hdi_bio_T1_post$CI_high, 3),   "]\n", median(mcmc_bio_T1_post$M_ratio),sep = "")
cat("Bio MC FB - Pre:  [", round(hdi_att_bio_T2_pre$CI_low, 3),   ", ", round(hdi_att_bio_T2_pre$CI_high, 3),   "]\n", median(mcmc_att_bio_T2_pre$M_ratio), sep = "")
cat("Bio MC FB - Post: [", round(hdi_att_bio_T2_post$CI_low, 3),  ", ", round(hdi_att_bio_T2_post$CI_high, 3),  "]\n", median(mcmc_att_bio_T2_post$M_ratio),sep = "")
cat("Bio MC Instr - Pre:  [", round(hdi_meta_bio_T2_pre$CI_low, 3),  ", ", round(hdi_meta_bio_T2_pre$CI_high, 3),  "]\n", median(mcmc_meta_bio_T2_pre$M_ratio), sep = "")
cat("Bio MC Instr - Post: [", round(hdi_meta_bio_T2_post$CI_low, 3), ", ", round(hdi_meta_bio_T2_post$CI_high, 3), "]\n", median(mcmc_meta_bio_T2_post$M_ratio),sep = "")

# --- DOTS ---
cat("Dots No FB - Pre:  [", round(hdi_dots_none_pre$CI_low, 3),  ", ", round(hdi_dots_none_pre$CI_high, 3),  "]\n", median(mcmc_dots_none_pre$M_ratio), sep = "")
cat("Dots No FB - Post: [", round(hdi_dots_none_post$CI_low, 3), ", ", round(hdi_dots_none_post$CI_high, 3), "]\n", median(mcmc_dots_none_post$M_ratio), sep = "")
cat("Dots Perf FB - Pre:  [", round(hdi_dots_T1_pre$CI_low, 3),    ", ", round(hdi_dots_T1_pre$CI_high, 3),    "]\n", median(mcmc_dots_T1_pre$M_ratio), sep = "")
cat("Dots Perf FB - Post: [", round(hdi_dots_T1_post$CI_low, 3),   ", ", round(hdi_dots_T1_post$CI_high, 3),   "]\n", median(mcmc_dots_T1_post$M_ratio), sep = "")
cat("Dots MC FB - Pre:  [", round(hdi_att_dots_T2_pre$CI_low, 3),   ", ", round(hdi_att_dots_T2_pre$CI_high, 3),   "]\n", median(mcmc_att_dots_T2_pre$M_ratio), sep = "")
cat("Dots MC FB - Post: [", round(hdi_att_dots_T2_post$CI_low, 3),  ", ", round(hdi_att_dots_T2_post$CI_high, 3),  "]\n", median(mcmc_att_dots_T2_post$M_ratio),sep = "")
cat("Dots MC Instr - Pre:  [", round(hdi_meta_dots_T2_pre$CI_low, 3),  ", ", round(hdi_meta_dots_T2_pre$CI_high, 3),  "]\n", median(mcmc_meta_dots_T2_pre$M_ratio), sep = "")
cat("Dots MC Instr - Post: [", round(hdi_meta_dots_T2_post$CI_low, 3), ", ", round(hdi_meta_dots_T2_post$CI_high, 3), "]\n", median(mcmc_meta_dots_T2_post$M_ratio), sep = "")

#### Contrasts of M-ratio between conditions at the Pre (Timepoint 1) timepoint####

#helper function
run_sexit <- function(posterior_a, posterior_b, label_a, label_b) {
  cat("---\n")
  cat("SEXIT:", label_a, "vs", label_b, "(Pre)\n")
  diff_vec <- posterior_a - posterior_b
  cat("  Mean difference (", label_a, " - ", label_b, "): ", round(mean(diff_vec), 3), "\n", sep = "")
  hdi_diff <- hdi(diff_vec, credMass = 0.95)
  cat("  95% HDI of difference: [", round(hdi_diff$CI_low, 3), ", ", round(hdi_diff$CI_high, 3), "]\n", sep = "")
  result <- sexit(diff_vec)
  print(result)
  cat("\n")
}

#first for Bio data
# T1 (Performance FB) vs None (No FB)
run_sexit(mcmc_bio_T1_pre$M_ratio,      mcmc_bio_none_pre$M_ratio,    "Bio Perf FB",      "Bio No FB")

# att_T2 (MC Feedback) vs T1 (Performance FB)
run_sexit(mcmc_att_bio_T2_pre$M_ratio,  mcmc_bio_T1_pre$M_ratio,      "Bio MC FB",  "Bio Perf FB")

#second for Dots

# T1 (Performance FB) vs None (No FB)
run_sexit(mcmc_dots_T1_pre$M_ratio,      mcmc_dots_none_pre$M_ratio,    "Dots Perf FB",      "Dots No FB")

# att_T2 (MC Feedback) vs T1 (Performance FB)
run_sexit(mcmc_att_dots_T2_pre$M_ratio,  mcmc_dots_T1_pre$M_ratio,      "Dots MC FB",  "Dots Perf FB")


#### The HDI differences from Timepoint 2 to Timepoint 1 ####
##This looks at the effects of training
#change in MC efficiency from pre to post calculated by subtracting mcmc vectors
#first for dots
mcmc_dots_none = bind_cols(group=mcmc_dots_none_post$group,diff=mcmc_dots_none_post$M_ratio-mcmc_dots_none_pre$M_ratio)
p17<-mcmc_dots_none %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "maroon") +
  labs(x = "M-ratio posterior estimate (Dots none Post - Pre)", y = "Density") +
  xlim(-0.5,0.75)+
  guides(fill="none")

mcmc_dots_T1 = bind_cols(group=mcmc_dots_T1_post$group,diff=mcmc_dots_T1_post$M_ratio-mcmc_dots_T1_pre$M_ratio)
p18<-mcmc_dots_T1 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "maroon") +
  labs(x = "M-ratio posterior estimate (Dots T1 Post - Pre)", y = "Density") +
  xlim(-0.5,0.75)+
  guides(fill="none")

mcmc_meta_dots_T2 = bind_cols(group=mcmc_meta_dots_T2_post$group,diff=mcmc_meta_dots_T2_post$M_ratio-mcmc_meta_dots_T2_pre$M_ratio)
p19<-mcmc_meta_dots_T2 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "maroon") +
  labs(x = "M-ratio posterior estimate (Dots Meta T2 Post - Pre)", y = "Density") +
  xlim(-0.5,0.75)+
  guides(fill="none")

mcmc_att_dots_T2 = bind_cols(group=mcmc_att_dots_T2_post$group,diff=mcmc_att_dots_T2_post$M_ratio-mcmc_att_dots_T2_pre$M_ratio)
p20<-mcmc_att_dots_T2 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "maroon") +
  labs(x = "M-ratio posterior estimate (Dots Att T2 Post - Pre)", y = "Density") +
  xlim(-0.5,0.75)+
  guides(fill="none")

combined_plot <- plot_grid(p17, p18, p19, p20, ncol = 1)
#ggsave(here("outputs", "dots_post_minus_pre_posteriors.pdf"), combined_plot, width = 10, height = 12)

#high density intervals for Post minus Pre - Dots
hdi_dots_none <- hdi(mcmc_dots_none$diff, credMass = 0.95)
#cat("dots none 95% HDI: [", round(hdi_dots_none[1], 3), ", ", round(hdi_dots_none[2], 3), "]\n", sep = "")
hdi_dots_T1 <- hdi(mcmc_dots_T1$diff, credMass = 0.95)
#cat("dots T1 95% HDI: [", round(hdi_dots_T1[1], 3), ", ", round(hdi_dots_T1[2], 3), "]\n", sep = "")
hdi_meta_dots_T2 <- hdi(mcmc_meta_dots_T2$diff, credMass = 0.95)
#cat("dots meta T2 95% HDI: [", round(hdi_meta_dots_T2[1], 3), ", ", round(hdi_meta_dots_T2[2], 3), "]\n", sep = "")
hdi_att_dots_T2 <- hdi(mcmc_att_dots_T2$diff, credMass = 0.95)
#cat("dots att T2 95% HDI: [", round(hdi_att_dots_T2[1], 3), ", ", round(hdi_att_dots_T2[2], 3), "]\n", sep = "")

#pre to post change for bio
mcmc_bio_none = bind_cols(group=mcmc_bio_none_post$group,diff=mcmc_bio_none_post$M_ratio-mcmc_bio_none_pre$M_ratio)
p21<-mcmc_bio_none %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "blue") +
  labs(x = "M-ratio posterior estimate (bio none Post - Pre)", y = "Density") +
  xlim(-1,1)+
  guides(fill="none")

mcmc_bio_T1 = bind_cols(group=mcmc_bio_T1_post$group,diff=mcmc_bio_T1_post$M_ratio-mcmc_bio_T1_pre$M_ratio)
p22<-mcmc_bio_T1 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "blue") +
  labs(x = "M-ratio posterior estimate (bio T1 Post - Pre)", y = "Density") +
  xlim(-1,1)+
  guides(fill="none")

mcmc_meta_bio_T2 = bind_cols(group=mcmc_meta_bio_T2_post$group,diff=mcmc_meta_bio_T2_post$M_ratio-mcmc_meta_bio_T2_pre$M_ratio)
p23<-mcmc_meta_bio_T2 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "blue") +
  labs(x = "M-ratio posterior estimate (bio Meta T2 Post - Pre)", y = "Density") +
  xlim(-1,1)+
  guides(fill="none")

mcmc_att_bio_T2 = bind_cols(group=mcmc_att_bio_T2_post$group,diff=mcmc_att_bio_T2_post$M_ratio-mcmc_att_bio_T2_pre$M_ratio)
p24<-mcmc_att_bio_T2 %>%
  ggplot(aes(diff)) +
  geom_density(alpha = 0.4, position = "identity", color = "blue") +
  labs(x = "M-ratio posterior estimate (bio Att T2 Post - Pre)", y = "Density") +
  xlim(-1,1)+
  guides(fill="none")

combined_plot <- plot_grid(p21, p22, p23, p24, ncol = 1)
#ggsave(here("outputs", "bio_post_minus_pre_posteriors.pdf"), combined_plot, width = 10, height = 12)

#high density intervals for Post minus Pre - Bio
hdi_bio_none <- hdi(mcmc_bio_none$diff, credMass = 0.95)
#cat("bio none 95% HDI: [", round(hdi_bio_none[1], 3), ", ", round(hdi_bio_none[2], 3), "]\n", sep = "")
hdi_bio_T1 <- hdi(mcmc_bio_T1$diff, credMass = 0.95)
#cat("bio T1 95% HDI: [", round(hdi_bio_T1[1], 3), ", ", round(hdi_bio_T1[2], 3), "]\n", sep = "")
hdi_meta_bio_T2 <- hdi(mcmc_meta_bio_T2$diff, credMass = 0.95)
#cat("bio meta T2 95% HDI: [", round(hdi_meta_bio_T2[1], 3), ", ", round(hdi_meta_bio_T2[2], 3), "]\n", sep = "")
hdi_att_bio_T2 <- hdi(mcmc_att_bio_T2$diff, credMass = 0.95)
#cat("bio att T2 95% HDI: [", round(hdi_att_bio_T2[1], 3), ", ", round(hdi_att_bio_T2[2], 3), "]\n", sep = "")

#### Examine the M-ratio differences between pre and post
sexit(mcmc_bio_none$diff)
sexit(mcmc_bio_T1$diff)
sexit(mcmc_att_bio_T2$diff)
sexit(mcmc_meta_bio_T2$diff)

sexit(mcmc_dots_none$diff)
sexit(mcmc_dots_T1$diff)
sexit(mcmc_att_dots_T2$diff)
sexit(mcmc_meta_dots_T2$diff)


#you can now return to the analysis R file


####___Visualizations of change in posterior distributions__ ####
#Results below are not included in manuscript and this can be skipped when building the combined code

#Does T1 differ from none
# Calculate Bayesian p-value (probability that one > the other)
prob_T1_none <- mean(mcmc_bio_T1$diff  > mcmc_bio_none$diff)
cat("\nProbability that bio T1 M-ratio > bio none post M-ratio: ", round(prob_T1_none * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_bio_T1$group <- "Bio T1"
mcmc_bio_none$group <- "Bio None"
combined_data <- bind_rows(mcmc_bio_T1, mcmc_bio_none)

# Create visualization showing both distributions and their HDIs
p25 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_bio_T1, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_bio_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Bio T1" = "blue", "Bio None" = "red"))

#Does T2 attention differ from none
prob_att_T2_none <- mean(mcmc_att_bio_T2$diff  > mcmc_bio_none$diff)
cat("\nProbability that attention bio T2 M-ratio > bio none post M-ratio: ", round(prob_att_T2_none * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_att_bio_T2$group <- "Att Bio T2"
mcmc_bio_none$group <- "Bio None"
combined_data <- bind_rows(mcmc_att_bio_T2, mcmc_bio_none)

# Create visualization showing both distributions and their HDIs
p26 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_att_bio_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_bio_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Att Bio T2" = "blue", "Bio None" = "red"))

#Does T2 attention differ from T1
prob_att_T2_T1 <- mean(mcmc_att_bio_T2$diff  > mcmc_bio_T1$diff)
cat("\nProbability that attention bio T2 M-ratio > bio T1 post M-ratio: ", round(prob_att_T2_T1 * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
combined_data <- bind_rows(mcmc_att_bio_T2, mcmc_bio_T1)

# Create visualization showing both distributions and their HDIs
p27 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_att_bio_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_bio_T1, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Att Bio T2" = "blue", "Bio T1" = "red"))

#Does T2 meta differ from T2 attention #This is not applicable
prob_att_meta_T2 <- mean(mcmc_meta_bio_T2$diff  >mcmc_att_bio_T2$diff)
cat("\nProbability that meta bio T2 M-ratio > att bio T2 M-ratio: ", round(prob_att_meta_T2 * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_meta_bio_T2$group <- "Meta Bio T2"
combined_data <- bind_rows(mcmc_att_bio_T2, mcmc_meta_bio_T2)

# Create visualization showing both distributions and their HDIs
p28 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  #geom_vline(xintercept = hdi_meta_bio_T2, linetype = "dashed", color = "blue") +
  #geom_vline(xintercept = hdi_bio_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Meta Bio T2" = "blue", "Bio None" = "red"))

#Does T2 meta differ from none
prob_meta_T2_none <- mean(mcmc_meta_bio_T2$diff  > mcmc_bio_none$diff)
cat("\nProbability that meta bio T2 M-ratio > bio none M-ratio: ", round(prob_meta_T2_none * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_meta_bio_T2$group <- "Meta Bio T2"
mcmc_bio_none$group <- "Bio None"
combined_data <- bind_rows(mcmc_meta_bio_T2, mcmc_bio_none)

# Create visualization showing both distributions and their HDIs
p33 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  #geom_vline(xintercept = hdi_meta_bio_T2, linetype = "dashed", color = "blue") +
  #geom_vline(xintercept = hdi_bio_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Meta Bio T2" = "blue", "Bio None" = "red"))

### contrasts for dots prompts
#Does T1 differ from none
# Calculate Bayesian p-value (probability that one > the other)
prob_T1_none_dots <- mean(mcmc_dots_T1$diff  > mcmc_dots_none$diff)
cat("\nProbability that dots T1 M-ratio > dots none post M-ratio: ", round(prob_T1_none_dots * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_dots_T1$group <- "Dots T1"
mcmc_dots_none$group <- "Dots None"
combined_data <- bind_rows(mcmc_dots_T1, mcmc_dots_none)

# Create visualization showing both distributions and their HDIs
p29 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_dots_T1, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_dots_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Dots T1" = "blue", "Dots None" = "red"))

#Does T2 attention differ from none
prob_att_T2_none_dots <- mean(mcmc_att_dots_T2$diff  > mcmc_dots_none$diff)
cat("\nProbability that attention dots T2 M-ratio > dots none post M-ratio: ", round(prob_att_T2_none_dots * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_att_dots_T2$group <- "Att Dots T2"
combined_data <- bind_rows(mcmc_att_dots_T2, mcmc_dots_none)

# Create visualization showing both distributions and their HDIs
p30 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_att_dots_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_dots_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Att Dots T2" = "blue", "Dots None" = "red"))

#Does T2 attention differ from T1
prob_att_T2_T1_dots <- mean(mcmc_att_dots_T2$diff  > mcmc_dots_T1$diff)
cat("\nProbability that attention dots T2 M-ratio > dots T1 post M-ratio: ", round(prob_att_T2_T1_dots * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
combined_data <- bind_rows(mcmc_att_dots_T2, mcmc_dots_T1)

# Create visualization showing both distributions and their HDIs
p31 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_att_dots_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_dots_T1, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Att Dots T2" = "blue", "Dots T1" = "red"))

#Does T2 attention differ from T2 meta #Not applicable
prob_att_meta_T2_dots <- mean(mcmc_att_dots_T2$diff  >mcmc_meta_dots_T2$diff)
cat("\nProbability that attention dots T2 M-ratio > dots T1 post M-ratio: ", round(prob_att_meta_T2_dots * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_meta_dots_T2$group <- "Meta Dots T2"
combined_data <- bind_rows(mcmc_att_dots_T2, mcmc_meta_dots_T2)

# Create visualization showing both distributions and their HDIs
p32 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_meta_dots_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_meta_dots_T2, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Meta Dots T2" = "blue", "Att Dots T2" = "red"))

#Does T2 meta differ from T2 none
prob_meta_T2_none_dots <- mean(mcmc_meta_dots_T2$diff  > mcmc_dots_none$diff)
cat("\nProbability that meta dots T2 M-ratio > dots none M-ratio: ", round(prob_meta_T2_none_dots * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_meta_dots_T2$group <- "Meta Dots T2"
mcmc_dots_none$group <- "Dots None"
combined_data <- bind_rows(mcmc_meta_dots_T2, mcmc_dots_none)

# Create visualization showing both distributions and their HDIs
p34 <- ggplot(combined_data, aes(x = diff, fill = group)) +
  geom_density(alpha = 0.5) +
  #geom_vline(xintercept = hdi_meta_dots_T2, linetype = "dashed", color = "blue") +
  #geom_vline(xintercept = hdi_dots_none, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("Meta Dots T2" = "blue", "Dots None" = "red"))


#Overlap calculation
#first the function
calculate_hdi_overlap <- function(hdi1, hdi2) {
  overlap <- min(hdi1$CI_high, hdi2$CI_high) - max(hdi1$CI_low, hdi2$CI_low)
  union_length <- max(hdi1$CI_high, hdi2$CI_high) - min(hdi1$CI_low, hdi2$CI_low)
  overlap_percentage <- (overlap / union_length) * 100

  return(list(
    overlap = overlap,
    percentage = overlap_percentage
  ))
}

#Change the HDI data set to compare distribution overlaps
result <- calculate_hdi_overlap(hdi_dots_none, hdi_meta_dots_T2)
cat("Overlap percentage:", round(result$percentage, 1), "%\n")

