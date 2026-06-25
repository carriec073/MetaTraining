# Load necessary libraries
library(tidyverse)
library(HDInterval)
library(here)
library(cowplot)

# Load the MCMC results
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


# Print the highest posterior density intervals (HDIs)
cat("bio none Pre 95% HDI: [", round(hdi_bio_none_pre[1], 3), ", ", round(hdi_bio_none_pre[2], 3), "]\n", sep = "")
cat("bio none Post 95% HDI: [", round(hdi_bio_none_post[1], 3), ", ", round(hdi_bio_none_post[2], 3), "]\n", sep = "")
cat("bio T1 Pre 95% HDI: [", round(hdi_bio_T1_pre[1], 3), ", ", round(hdi_bio_T1_pre[2], 3), "]\n", sep = "")
cat("bio T1 Post 95% HDI: [", round(hdi_bio_T1_post[1], 3), ", ", round(hdi_bio_T1_post[2], 3), "]\n", sep = "")
cat("bio T2 meta Pre 95% HDI: [", round(hdi_meta_bio_T2_pre[1], 3), ", ", round(hdi_meta_bio_T2_pre[2], 3), "]\n", sep = "")
cat("bio T2 meta Post 95% HDI: [", round(hdi_meta_bio_T2_post[1], 3), ", ", round(hdi_meta_bio_T2_post[2], 3), "]\n", sep = "")
cat("bio T2 att Pre 95% HDI: [", round(hdi_att_bio_T2_pre[1], 3), ", ", round(hdi_att_bio_T2_pre[2], 3), "]\n", sep = "")
cat("bio T2 att Post 95% HDI: [", round(hdi_att_bio_T2_post[1], 3), ", ", round(hdi_att_bio_T2_post[2], 3), "]\n", sep = "")

cat("dots none Pre 95% HDI: [", round(hdi_dots_none_pre[1], 3), ", ", round(hdi_dots_none_pre[2], 3), "]\n", sep = "")
cat("dots none Post 95% HDI: [", round(hdi_dots_none_post[1], 3), ", ", round(hdi_dots_none_post[2], 3), "]\n", sep = "")
cat("dots T1 Pre 95% HDI: [", round(hdi_dots_T1_pre[1], 3), ", ", round(hdi_dots_T1_pre[2], 3), "]\n", sep = "")
cat("dots T1 Post 95% HDI: [", round(hdi_dots_T1_post[1], 3), ", ", round(hdi_dots_T1_post[2], 3), "]\n", sep = "")
cat("dots T2 meta Pre 95% HDI: [", round(hdi_meta_dots_T2_pre[1], 3), ", ", round(hdi_meta_dots_T2_pre[2], 3), "]\n", sep = "")
cat("dots T2 meta Post 95% HDI: [", round(hdi_meta_dots_T2_post[1], 3), ", ", round(hdi_meta_dots_T2_post[2], 3), "]\n", sep = "")
cat("dots T2 att Pre 95% HDI: [", round(hdi_att_dots_T2_pre[1], 3), ", ", round(hdi_att_dots_T2_pre[2], 3), "]\n", sep = "")
cat("dots T2 att Post 95% HDI: [", round(hdi_att_dots_T2_post[1], 3), ", ", round(hdi_att_dots_T2_post[2], 3), "]\n", sep = "")


#change from pre to post calculated by subtracting mcmc vectors
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
ggsave(here("outputs", "dots_post_minus_pre_posteriors.pdf"), combined_plot, width = 10, height = 12)

#high density intervals for Post minus Pre - Dots
hdi_dots_none <- hdi(mcmc_dots_none$diff, credMass = 0.95)
cat("dots none 95% HDI: [", round(hdi_dots_none[1], 3), ", ", round(hdi_dots_none[2], 3), "]\n", sep = "")
hdi_dots_T1 <- hdi(mcmc_dots_T1$diff, credMass = 0.95)
cat("dots T1 95% HDI: [", round(hdi_dots_T1[1], 3), ", ", round(hdi_dots_T1[2], 3), "]\n", sep = "")
hdi_meta_dots_T2 <- hdi(mcmc_meta_dots_T2$diff, credMass = 0.95)
cat("dots meta T2 95% HDI: [", round(hdi_meta_dots_T2[1], 3), ", ", round(hdi_meta_dots_T2[2], 3), "]\n", sep = "")
hdi_att_dots_T2 <- hdi(mcmc_att_dots_T2$diff, credMass = 0.95)
cat("dots att T2 95% HDI: [", round(hdi_att_dots_T2[1], 3), ", ", round(hdi_att_dots_T2[2], 3), "]\n", sep = "")

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
ggsave(here("outputs", "bio_post_minus_pre_posteriors.pdf"), combined_plot, width = 10, height = 12)

#high density intervals for Post minus Pre - Bio
hdi_bio_none <- hdi(mcmc_bio_none$diff, credMass = 0.95)
cat("bio none 95% HDI: [", round(hdi_bio_none[1], 3), ", ", round(hdi_bio_none[2], 3), "]\n", sep = "")
hdi_bio_T1 <- hdi(mcmc_bio_T1$diff, credMass = 0.95)
cat("bio T1 95% HDI: [", round(hdi_bio_T1[1], 3), ", ", round(hdi_bio_T1[2], 3), "]\n", sep = "")
hdi_meta_bio_T2 <- hdi(mcmc_meta_bio_T2$diff, credMass = 0.95)
cat("bio meta T2 95% HDI: [", round(hdi_meta_bio_T2[1], 3), ", ", round(hdi_meta_bio_T2[2], 3), "]\n", sep = "")
hdi_att_bio_T2 <- hdi(mcmc_att_bio_T2$diff, credMass = 0.95)
cat("bio att T2 95% HDI: [", round(hdi_att_bio_T2[1], 3), ", ", round(hdi_att_bio_T2[2], 3), "]\n", sep = "")

#### Contrasts of Treatments ###

#first for biology prompts
#Does T1 differ from none
# Calculate Bayesian p-value (probability that one > the other)
prob_T1_none <- mean(mcmc_bio_T1$diff  > mcmc_bio_none$diff)
cat("\nProbability that bio T1 M-ratio > bio none post M-ratio: ", round(prob_T1_none * 100, 1), "%\n", sep = "")

#Does T2 attention differ from none
prob_att_T2_none <- mean(mcmc_att_bio_T2$diff  > mcmc_bio_none$diff)
cat("\nProbability that attention bio T2 M-ratio > bio none post M-ratio: ", round(prob_att_T2_none * 100, 1), "%\n", sep = "")

#Does T2 attention differ from T1
prob_att_T2_T1 <- mean(mcmc_att_bio_T2$diff  > mcmc_bio_T1$diff)
cat("\nProbability that attention bio T2 M-ratio > bio T1 post M-ratio: ", round(prob_att_T2_T1 * 100, 1), "%\n", sep = "")

#Does T2 meta differ from T1 meta
prob_att_meta_T2 <- mean(mcmc_meta_bio_T2$diff  >mcmc_att_bio_T2$diff)
cat("\nProbability that meta bio T2 M-ratio > att bio T1 post M-ratio: ", round(prob_att_meta_T2 * 100, 1), "%\n", sep = "")

#contrasts for dots prompts
#Does T1 differ from none
# Calculate Bayesian p-value (probability that one > the other)
prob_T1_none_dots <- mean(mcmc_dots_T1$diff  > mcmc_dots_none$diff)
cat("\nProbability that dots T1 M-ratio > dots none post M-ratio: ", round(prob_T1_none_dots * 100, 1), "%\n", sep = "")

#Does T2 attention differ from none
prob_att_T2_none_dots <- mean(mcmc_att_dots_T2$diff  > mcmc_dots_none$diff)
cat("\nProbability that attention dots T2 M-ratio > dots none post M-ratio: ", round(prob_att_T2_none_dots * 100, 1), "%\n", sep = "")

#Does T2 attention differ from T1
prob_att_T2_T1_dots <- mean(mcmc_att_dots_T2$diff  > mcmc_dots_T1$diff)
cat("\nProbability that attention dots T2 M-ratio > dots T1 post M-ratio: ", round(prob_att_T2_T1_dots * 100, 1), "%\n", sep = "")

#Does T2 attention differ from T2 meta
prob_att_meta_T2_dots <- mean(mcmc_att_dots_T2$diff  >mcmc_meta_dots_T2$diff)
cat("\nProbability that attention dots T2 M-ratio > dots T1 post M-ratio: ", round(prob_att_meta_T2_dots * 100, 1), "%\n", sep = "")

#### Outcomes for Bayesian statistics
library(bayestestR)
sexit(mcmc_bio_T1$diff  - mcmc_bio_none$diff)
sexit(mcmc_att_dots_T2$diff  - mcmc_dots_none$diff)

##### Check if the HDIs overlap
overlap <- min(hdi_meta_bio_T2[2], hdi_att_bio_T2[2]) - max(hdi_meta_bio_T2[1], hdi_att_bio_T2[1])
overlap_exists <- overlap > 0


cat("\nDo the 95% HDIs overlap? ", ifelse(overlap_exists, "Yes", "No"), "\n", sep = "")
if(overlap_exists) {
  cat("Amount of overlap: ", round(overlap, 3), "\n", sep = "")

  # Calculate the percentage of overlap relative to the union of both intervals
  union_length <- max(hdi_meta_bio_T2[2], hdi_att_bio_T2[2]) - min(hdi_meta_bio_T2[1], hdi_att_bio_T2[1])
  overlap_percentage <- (overlap / union_length) * 100
  cat("Percentage of overlap: ", round(overlap_percentage, 1), "%\n", sep = "")
}

# Calculate Bayesian p-value (probability that one > the other)
prob_meta_att_greater <- mean(mcmc_bio_T1_pre$M_ratio  > mcmc_bio_T1_post$M_ratio)
cat("\nProbability that att bio T1 pre M-ratio > att bio T1 post M-ratio: ", round(prob_meta_att_greater * 100, 1), "%\n", sep = "")

# Combine datasets for plotting
mcmc_att_bio_T2$task <- "att bio T2"
mcmc_meta_bio_T2$task <- "meta bio T2"
combined_data <- bind_rows(mcmc_att_bio_T2, mcmc_meta_bio_T2)

# Create visualization showing both distributions and their HDIs
p1 <- ggplot(combined_data, aes(x = M_ratio, fill = task)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = hdi_att_bio_T2, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = hdi_meta_bio_T2, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distributions of M-ratio with 95% HDIs",
    x = "M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("meta bio T2" = "red", "att bio T2" = "blue"))

# Add annotations for HDI ranges
p1 <- p1 +
  annotate("text", x = mean(hdi_att_bio_T2), y = 0.1,
           label = paste0("dots T1 HDI: [", round(hdi_att_bio_T2[1], 2), ", ", round(hdi_att_bio_T2[2], 2), "]"),
           color = "blue", size = 3) +
  annotate("text", x = mean(hdi_meta_bio_T2), y = 0.2,
           label = paste0("dots None HDI: [", round(hdi_meta_bio_T2[1], 2), ", ", round(hdi_meta_bio_T2[2], 2), "]"),
           color = "red", size = 3)

# Save the visualization
ggsave(here("functions", "output", "hdi_bio_T2_meta_att_comparison_plot.pdf"), p1, width = 10, height = 6)

# Create another plot showing the difference distribution
# Calculate the difference between bio and dots for each MCMC iteration
set.seed(42) # For reproducibility when sampling
sample_size <- min(nrow(mcmc_bio_T1), nrow(mcmc_dots_T1))
diff_samples <- mcmc_bio_T1$M_ratio[1:sample_size] - mcmc_dots_T1$M_ratio[1:sample_size]
diff_df <- data.frame(difference = diff_samples)

# Calculate HDI of the difference
hdi_diff <- hdi(diff_samples, credMass = 0.95)
cat("\nDifference (Bio - Dots) 95% HDI: [", round(hdi_diff[1], 3), ", ", round(hdi_diff[2], 3), "]\n", sep = "")
cat("Does the difference HDI include zero? ", ifelse(hdi_diff[1] <= 0 && hdi_diff[2] >= 0, "Yes", "No"), "\n", sep = "")

# Plot the difference distribution
p2 <- ggplot(diff_df, aes(x = difference)) +
  geom_density(fill = "purple", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = hdi_diff, linetype = "dotted", color = "darkred") +
  labs(
    title = "Posterior Distribution of Difference (Bio - Dots) with 95% HDI",
    x = "Difference in M-ratio",
    y = "Density"
  ) +
  theme_minimal() +
  annotate("text", x = mean(hdi_diff), y = 0.1,
           label = paste0("95% HDI: [", round(hdi_diff[1], 2), ", ", round(hdi_diff[2], 2), "]"),
           color = "darkred", size = 3)

# Save the difference plot
ggsave(here("functions", "output", "difference_hdi_plot.pdf"), p2, width = 10, height = 6)

# Create a combined figure with both plots
combined_plot <- plot_grid(p1, p2, ncol = 1, labels = c("A", "B"))
ggsave(here("functions", "output", "combined_hdi_analysis.pdf"), combined_plot, width = 10, height = 12)

# Save the HDI results to a CSV file
hdi_results <- data.frame(
  Task = c("Dots T1", "Bio T1", "Difference (Bio-Dots)"),
  Lower_HDI = c(hdi_dots[1], hdi_bio[1], hdi_diff[1]),
  Upper_HDI = c(hdi_dots[2], hdi_bio[2], hdi_diff[2]),
  Mean = c(mean(mcmc_dots_T1$M_ratio), mean(mcmc_bio_T1$M_ratio), mean(diff_samples)),
  Median = c(median(mcmc_dots_T1$M_ratio), median(mcmc_bio_T1$M_ratio), median(diff_samples))
)

write.csv(hdi_results, file = here("functions", "output", "hdi_compa
