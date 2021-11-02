# Helpers -----------------------------------------------------------------

source("univar_ar1/full_conditionals.R")

# Gibbs sampler parameters ------------------------------------------------

n_sim    <- 1e4
n_burnin <- n_sim / 2
verbose  <- n_sim / 20

# Storage arrays ----------------------------------------------------------

sigma2_w_array <- numeric(n_sim - n_burnin)
phi_array      <- numeric(n_sim - n_burnin)

# Initialization ----------------------------------------------------------

sigma2_w <- invgamma::rinvgamma(n = 1, shape = 3, rate = 1)
phi      <- truncnorm::rtruncnorm(n = 1, a = -1, b = 1)

# Sampling ----------------------------------------------------------------

for (iter in 1:n_sim) {
  
  sigma2_w <- sample_sigma2_w(
    y           = y,
    phi         = phi,
    prior_shape = 3,
    prior_rate  = 1
  )

  phi <- sample_phi(
    y          = y,
    sigma2_w   = sigma2_w,
    sigma2_phi = 10
  )
  
  if (iter > n_burnin) {
    sigma2_w_array[iter - n_burnin] <- sigma2_w
    phi_array[iter - n_burnin]      <- phi
  }
  
  if (iter %% verbose == 0 & iter <= n_burnin) {
    cat(paste0("Iteration: ", iter, " (burn in).\n"))
  } else if (iter %% verbose == 0 & iter > n_burnin) {
    cat(paste0("Iteration: ", iter, " (sampling).\n"))
  }
}

post_samples <- list(
  sigma2_w = sigma2_w_array,
  phi      = phi_array
)
