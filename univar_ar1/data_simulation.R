# Parameters --------------------------------------------------------------

n             <- 1000 # length of time series
true_phi      <- 0.9 # parameter of AR(1) model
true_sigma2_w <- 0.2 # variance of the white noise

# Time series -------------------------------------------------------------

y <- arima.sim(
  model = list(order = c(1, 0, 0), ar = true_phi),
  n     = n,
  sd    = sqrt(true_sigma2_w)
)

# Plots -------------------------------------------------------------------

par(mfrow = c(1, 1))
plot.ts(y, type = "o", pch = 19, cex = 0.5, lwd = 1.5)
abline(h = 0, lty = 2, col = "gray")

# Diagnostics -------------------------------------------------------------

arima(tail(y, n), order = c(1, 0, 0))

# plot(
#   true_phi ^ (0:(length(acf(y, plot = FALSE)$acf[, , 1]) - 1)),
#   acf(y, plot = FALSE)$acf[, , 1],
#   xlab = "true acf",
#   ylab = "est. acf",
#   pch  = 20
# )
# abline(a = 0, b = 1, lty = 2)

