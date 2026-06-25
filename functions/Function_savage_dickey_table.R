#### SAVAGE-DICKEY BAYES FACTOR TABLE GENERATOR ####
# Generate a data table of Bayes factors for various prior specifications

# Function to calculate Bayes factor for a single prior specification
calculate_BF <- function(difference, null_value = 0, prior_mean = 0, prior_sd = 1) {

  # Estimate posterior density at null value
  posterior_density <- density(difference)
  posterior_at_null <- approx(posterior_density$x, posterior_density$y,
                              xo = null_value)$y

  # Calculate prior density at null value
  prior_at_null <- dnorm(null_value, mean = prior_mean, sd = prior_sd)

  # Calculate Bayes Factor (BF01)
  BF01 <- posterior_at_null / prior_at_null

  return(BF01)
}


# Main function to generate table of Bayes factors
savage_dickey_table <- function(posterior_pre, posterior_post,
                                null_value = 0,
                                prior_means = seq(0, 0.9, by = 0.1),
                                prior_sds = c(1, 2)) {

  # Extract posterior samples for mu_logMratio from each timepoint
  samples_pre <- as.numeric(as.matrix(posterior_pre)[, "value"])
  samples_post <- as.numeric(as.matrix(posterior_post)[, "value"])

  # Calculate the difference: Post - Pre
  difference <- samples_post - samples_pre

  # Create empty list to store results
  results_list <- list()

  # Loop through all combinations of prior parameters
  for (prior_sd in prior_sds) {
    for (prior_mean in prior_means) {

      # Calculate Bayes factor for this prior specification
      BF01 <- calculate_BF(difference, null_value, prior_mean, prior_sd)

      # Determine interpretation
      interpretation <- ifelse(BF01 < 0.33,
                               "Evidence for null (Pre == Post)",
                               ifelse(BF01 > 3,
                                      "Evidence for difference (Pre != Post)",
                                      "Inconclusive"))

      # Store results
      results_list[[length(results_list) + 1]] <- data.frame(
        prior_mean = prior_mean,
        prior_sd = prior_sd,
        BF01 = BF01,
        log_BF01 = log(BF01),
        interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all results into a single data frame
  results_table <- do.call(rbind, results_list)

  # Add additional summary information
  attr(results_table, "null_value") <- null_value
  attr(results_table, "n_samples") <- length(difference)
  attr(results_table, "difference_mean") <- mean(difference)
  attr(results_table, "difference_sd") <- sd(difference)

  return(results_table)
}


# Helper function to print the table nicely
print_savage_dickey_table <- function(results_table) {

  cat("\n=== SAVAGE-DICKEY BAYES FACTOR TABLE ===\n\n")

  cat("Analysis Summary:\n")
  cat(sprintf("  Null value tested: %.2f\n", attr(results_table, "null_value")))
  cat(sprintf("  Number of posterior samples: %d\n", attr(results_table, "n_samples")))
  cat(sprintf("  Mean difference: %.4f\n", attr(results_table, "difference_mean")))
  cat(sprintf("  SD of difference: %.4f\n\n", attr(results_table, "difference_sd")))

  cat("Bayes Factors:\n")
  cat("  BF01 < 0.33: Evidence FOR null hypothesis\n")
  cat("  BF01 > 3.00: Evidence AGAINST null hypothesis\n")
  cat("  0.33 < BF01 < 3: Inconclusive\n\n")

  print(results_table, row.names = FALSE)

  cat("\n")
}

