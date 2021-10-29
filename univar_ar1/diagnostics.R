# Helpers -----------------------------------------------------------------

source("data_simulation.R")
source("full_conditionals.R")
source("gibbs_sampler.R")

# Extract posterior samples -----------------------------------------------

sigma2_w <- post_samples$sigma2_w
phi      <- post_samples$phi

# Posterior Summaries -----------------------------------------------------

post_df <- cbind(sigma2_w, phi)
colnames(post_df) <- c("sigma2_w", "phi")

post_summary <- data.frame(
  low_CI = apply(post_df, 2, function(x) round(quantile(x, 0.025), 4)),
  mean   = apply(post_df, 2, function(x) round(mean(x), 4)),
  upp_CI = apply(post_df, 2, function(x) round(quantile(x, 0.975), 4))
)

post_summary

# Traceplots --------------------------------------------------------------

par(mfrow = c(2, 1))

plot.ts(
  sigma2_w, 
  main = paste0(
    "Mean: ", post_summary$mean[1],
    "\n",
    "95% CI: (",  post_summary$low_CI[1], ", ", post_summary$upp_CI[1], ")"
  ),
  lwd = 1,
  xlab = "",
  ylab = expression(sigma[w]^2)
)
abline(h = true_sigma2_w, lwd = 2, lty = 2, col = "red")

plot.ts(
  phi, 
  main = paste0(
    "Mean: ", post_summary$mean[2],
    "\n",
    "95% CI: (",  post_summary$low_CI[2], ", ", post_summary$upp_CI[2], ")"
  ),
  lwd = 1,
  xlab = "",
  ylab = expression(phi)
)
abline(h = true_phi, lwd = 2, lty = 2, col = "red")

