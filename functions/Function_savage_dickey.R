#### PART 2: SAVAGE-DICKEY DENSITY RATIO TEST ####
# Compare Pre vs Post timepoints

# Function to perform Savage-Dickey density ratio test
savage_dickey_test <- function(posterior_pre, posterior_post, null_value = 0) {

  # Extract posterior samples for mu_logMratio from each timepoint
  samples_pre <- as.numeric(as.matrix(posterior_pre)[, "value"])
  samples_post <- as.numeric(as.matrix(posterior_post)[, "value"])

  # Calculate the difference: Post - Pre
  difference <- samples_post - samples_pre

  # Estimate posterior density at null value (0 difference)
  posterior_density <- density(difference)
  posterior_at_null <- approx(posterior_density$x, posterior_density$y,
                              xo = null_value)$y

  # Estimate prior density at null value
  # Assuming a diffuse normal prior: N(0, sigma)
  # We need to estimate sigma from the data
  prior_sd <- sd(difference) * 2  # Conservative prior
  prior_at_null <- dnorm(null_value, mean = 0, sd = prior_sd)

  # Calculate Bayes Factor (BF01)
  # BF01 = posterior density at null / prior density at null
  BF01 <- posterior_at_null / prior_at_null

  # Return results
  return(list(
    BF01 = BF01,
    #difference_samples = difference,
    posterior_at_null = posterior_at_null,
    prior_at_null = prior_at_null,
    interpretation = ifelse(BF01 < 0.33,
                            "Evidence for null (Pre == Post)",
                            ifelse(BF01 > 3,
                                   "Evidence for difference (Pre != Post)",
                                   "Inconclusive"))
  ))
}
