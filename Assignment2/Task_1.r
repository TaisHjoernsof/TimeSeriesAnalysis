phi_1 <- -0.7
phi_2 <- -0.2

rho <- function(k) {
  rho <- list()

  rho[[1]] <- 1
  rho[[2]] <- -(phi_1 / (1 + phi_2))
  rho[[3]] <- -(rho[[2]] * phi_1 + phi_2)

  for (i in 4:(k + 1)) {
    rho[[i]] <- -(rho[[i - 1]] * phi_1 + rho[[i - 2]] * phi_2)
  }
  return(rho)
}

# Custom ACF values and lags (lag 0 included)
lags = 30
my_lags <- 0:lags
my_acf <- rho(lags)

# Plot vertical lines
plot(my_lags, my_acf, type = "h", lwd = 2, ylim = c(-0.1, 1),
     xlab = "Lag", ylab = "ACF", main = "Custom ACF Plot")
abline(h = 0)