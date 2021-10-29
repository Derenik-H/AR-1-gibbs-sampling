# sigma2_w ----------------------------------------------------------------

sample_sigma2_w <- function(
  y,
  phi,
  prior_shape = 3,
  prior_rate = 1
) {
  
  n <- length(y)
  
  post_shape <- prior_shape + (n - 1) / 2
  post_rate  <- prior_rate + 0.5 * sum((y[-1] - phi * y[-n]) ^ 2)
  
  invgamma::rinvgamma(
    n     = 1,
    shape = post_shape,
    rate  = post_rate
  )
}

# phi ---------------------------------------------------------------------

sample_phi <- function(
  y,
  sigma2_w,
  sigma2_phi = 1
) {

  n <- length(y)
  
  temp1 <- sum(y[-n] ^ 2)
  temp2 <- sum(y[-1] * y[-n])
  
  post_var  <- (sigma2_w * sigma2_phi) / (sigma2_phi * temp1 + sigma2_w)
  post_mean <- post_var * (temp2 / sigma2_w)
  
  truncnorm::rtruncnorm(
    n    = 1,
    a    = -1,
    b    = 1,
    mean = post_mean,
    sd   = sqrt(post_var)
  )
}
