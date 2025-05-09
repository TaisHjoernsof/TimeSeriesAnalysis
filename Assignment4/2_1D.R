library(ggplot2)
library(ggfortify)   # For autoplot on ACF/PACF
library(patchwork)   # For combining plots

# Load functions
source("functions/functions_exercise2.R")
source("functions/plot_functions.R")

# Apply global plot style
set_plot_style()


start_par <- c(
  A  = 0.9,
  B1 = 0.1,
  B2 = 0.01,
  B3 = 0.1,
  q  = 1,
  r  = 1,
  X0 = 20
)

# Lower bounds (to ensure stability and positive variances)
lower <- c(
  A  = -1,
  B1 = -10,
  B2 = -10,
  B3 = -10,
  q  = 1e-4,
  r  = 1e-4,
  X0 = 0
)

# Optional: add upper bounds
upper <- c(
  A  = 1,
  B1 = 10,
  B2 = 10,
  B3 = 10,
  q  = 100,
  r  = 100,
  X0 = 50
)

# estimate and optimise 
result <- estimate_dt(start_par, df, lower = lower, upper = upper)
# View the estimated parameter vector
names(result$par) <- c("A", "B1", "B2", "B3", "q", "r", "X0")
print(result$par)

# Negative log-likelihood (value minimized)
result$value

# helper function applying kalman filter 
kf_filter_apply <- function(par, df) {
  A <- par[1]
  B <- matrix(par[2:4], nrow = 1)
  q <- par[5]
  r <- par[6]
  x0 <- matrix(par[7], nrow = 1)
  C <- matrix(1, nrow = 1)
  
  Y <- as.matrix(df$Y)
  U <- as.matrix(df[, c("Ta", "S", "I")])
  Tn <- nrow(Y)
  
  x_est <- x0
  P_est <- matrix(10)
  
  y_preds <- numeric(Tn)
  innovations <- numeric(Tn)
  x_filtered <- numeric(Tn)
  
  for (t in 1:Tn) {
    x_pred <- A * x_est + B %*% matrix(U[t, ], ncol = 1)
    P_pred <- A * P_est * A + q
    
    y_pred <- C %*% x_pred
    S_t <- C %*% P_pred %*% t(C) + r
    innov <- Y[t] - y_pred
    
    K_t <- P_pred %*% t(C) %*% solve(S_t)
    x_est <- x_pred + K_t %*% innov
    P_est <- (1 - K_t %*% C) * P_pred
    
    y_preds[t] <- y_pred
    innovations[t] <- innov
    x_filtered[t] <- x_est
  }
  
  return(list(
    y_pred = y_preds,
    residuals = innovations,
    x_filtered = x_filtered
  ))
}

kf_result <- kf_filter_apply(result$par, df)

plot(kf_result$residuals, type = "l")
acf(kf_result$residuals)
pacf(kf_result$residuals)
qqnorm(kf_result$residuals); qqline(kf_result$residuals)

# Residual vector and dataframe
resid <- kf_result$residuals
resid_df <- data.frame(time = 1:length(resid), residual = resid)

# Residual time series plot
p1 <- ggplot(resid_df, aes(x = time, y = residual)) +
  geom_line(color = "steelblue") +
  labs(title = "Residuals Over Time", x = "Time (hours)", y = "Residual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ACF
p2 <- autoplot(acf(resid, plot = FALSE)) +
  labs(title = "ACF") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# PACF
p3 <- autoplot(pacf(resid, plot = FALSE)) +
  labs(title = "PACF") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# QQ-plot
p4 <- ggplot(data.frame(sample = resid), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ-Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Combine plots using patchwork: 2x2 layout
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Residual Diagnostics for 1D State-Space Model",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

# Display it
combined_plot


compare_df <- data.frame(
  time = df$time,
  observed = df$Y,
  predicted = kf_result$y_pred
)

ggplot(compare_df, aes(x = time)) +
  geom_line(aes(y = observed, color = "Observed"), size = 1) +
  geom_line(aes(y = predicted, color = "Modelled"), size = 1, linetype = "dashed") +
  labs(
    title = "Model Fit: Modelled vs. Observed Transformer Temperature",
    x = "Time (hours)",
    y = "Temperature (°C)",
    color = NULL
  ) +
  scale_color_manual(values = c("Observed" = "black", "Modelled" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.043, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

compare_df <- data.frame(
  observed = df$Y,
  modelled = kf_result$y_pred
)
rmse <- sqrt(mean((compare_df$observed - compare_df$modelled)^2))
# Normalizers
mean_Y <- mean(compare_df$observed)
range_Y <- diff(range(compare_df$observed))
sd_Y <- sd(compare_df$observed)

# Normalized RMSEs
nrmse_mean  <- rmse / mean_Y
nrmse_range <- rmse / range_Y
nrmse_sd    <- rmse / sd_Y

# Display
cat("RMSE:", rmse, "\n")
cat("nRMSE (mean):", round(nrmse_mean, 3), "\n")
cat("nRMSE (range):", round(nrmse_range, 3), "\n")
cat("nRMSE (std dev):", round(nrmse_sd, 3), "\n")

# Extract values
logLik <- -result$value      # log-likelihood is the negative of value
k <- length(result$par)      # number of parameters
n <- nrow(df)                   # number of observations

# Calculate
aic <- -2 * logLik + 2 * k
bic <- -2 * logLik + log(n) * k

# Print
cat("AIC:", round(aic, 2), "\n")
cat("BIC:", round(bic, 2), "\n")



