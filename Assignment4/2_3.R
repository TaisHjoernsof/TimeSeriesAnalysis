library(ggplot2)
library(ggfortify)   # For autoplot on ACF/PACF
library(patchwork)   # For combining plots

# Load functions
source("functions/functions_exercise2.R")
source("functions/plot_functions.R")

# Apply global plot style
set_plot_style()

# add bias to input data
df$bias <- 1

# Start values (A, B, Q_lt, R, X0)
start_par_2d <- c(
  # A matrix (2x2)
  0.8,  0.1,
  0.01, 0.2,
  
  # B matrix (2x3)
  0.1, 0.01, 0.2, 
  0.01, 0.01, 0.01,
  
  # Q lower-triangle entries (Cholesky of 2x2)
  0.2, 0.05, 0.2,
  
  # R
  0.01,
  
  # X0 (initial state: temperature around 25°C, latent near 0)
  25, 0
)



lower_2d <- c(
  rep(-1.5, 4),     # A
  rep(0, 3),        # B11–B13 (first state, physically positive)
  rep(-1, 3),       # B21–B23 (latent)
  1e-4, -5, 1e-4,   # Q lower-triangle
  0.001,            # R
  0, -100           # X0: temp ≥ 0°C, latent free
)



upper_2d <- c(
  rep(1.5, 4),
  rep(1, 6),        # B matrix
  5, 5, 5,          # Q_lt
  1,                # R
  50, 100           # X0: temp ≤ 50°C
)


df

result_2d <- estimate_dt_2d(start_par_2d, df, lower = lower_2d, upper = upper_2d)


names(result_2d$par) <- c(
  "A11", "A12", "A21", "A22",
  "B11", "B12", "B13", "B21", "B22", "B23",
  "Q11", "Q21", "Q22",
  "R"
)
round(result_2d$par, 4)

kf_filter_apply_2d <- function(par, df) {
  # Unpack parameters
  A <- matrix(par[1:4], nrow = 2, byrow = TRUE)
  B <- matrix(par[5:10], nrow = 2, byrow = TRUE)
  Q_lt <- matrix(c(par[11], 0, par[12], par[13]), nrow = 2, byrow = TRUE)
  Q <- Q_lt %*% t(Q_lt)
  R <- par[14]
  X0 <- matrix(par[15:16], nrow = 2)  # now estimated
  
  C <- matrix(c(1, 0), nrow = 1)
  
  # Data
  Y <- as.matrix(df$Y)
  U <- as.matrix(df[, c("Ta", "S", "I")])
  Tn <- nrow(Y)
  
  # Init
  x_est <- X0
  P_est <- diag(10, 2)
  
  y_preds <- numeric(Tn)
  residuals <- numeric(Tn)
  x_filtered <- matrix(NA, nrow = Tn, ncol = 2)
  
  for (t in 1:Tn) {
    u_t <- matrix(U[t, ], ncol = 1)
    
    # Predict
    x_pred <- A %*% x_est + B %*% u_t
    P_pred <- A %*% P_est %*% t(A) + Q
    
    # Innovation
    y_pred <- C %*% x_pred
    S_t <- C %*% P_pred %*% t(C) + R
    if (S_t <= 0 || !is.finite(S_t)) return(-1e6)  # prevent crash
    
    innov <- Y[t] - y_pred
    
    # Update
    K_t <- P_pred %*% t(C) %*% solve(S_t)
    x_est <- x_pred + K_t %*% innov
    P_est <- (diag(2) - K_t %*% C) %*% P_pred
    
    # Store results
    y_preds[t] <- y_pred
    residuals[t] <- innov
    x_filtered[t, ] <- x_est
  }
  
  return(list(
    y_pred = y_preds,
    residuals = residuals,
    x_filtered = x_filtered
  ))
}


kf_result_2d <- kf_filter_apply_2d(result_2d$par, df)

compare_2d_df <- data.frame(
  time     = df$time,
  observed = df$Y,
  modelled = kf_result_2d$y_pred
)

ggplot(compare_2d_df, aes(x = time)) +
  geom_line(aes(y = observed, color = "Observed"), size = 1) +
  geom_line(aes(y = modelled, color = "Modelled"), size = 1, linetype = "dashed") +
  labs(
    title = "2D State-Space Model Fit",
    x = "Time (hours)",
    y = "Transformer Temperature (°C)",
    color = NULL
  ) +
  scale_color_manual(values = c("Observed" = "black", "Modelled" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

# debug
print(round(result_2d$par, 4))
range(kf_result_2d$y_pred)
range(df$Y)

print(result_2d$par[15:16])

library(ggplot2)
library(ggfortify)
library(patchwork)

# Residual vector
resid <- kf_result_2d$residuals
resid_df <- data.frame(time = df$time, residual = resid)

# Plot 1: Residuals over time
p1 <- ggplot(resid_df, aes(x = time, y = residual)) +
  geom_line(color = "steelblue") +
  labs(title = "Residuals Over Time", x = "Time (hours)", y = "Residual") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 2: ACF
p2 <- autoplot(acf(resid, plot = FALSE)) +
  labs(title = "ACF") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 3: PACF
p3 <- autoplot(pacf(resid, plot = FALSE)) +
  labs(title = "PACF") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Plot 4: QQ-plot
p4 <- ggplot(data.frame(sample = resid), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "QQ Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Combine plots using patchwork: 2x2 layout
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Residual Diagnostics for 2D State-Space Model",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

# Display it
combined_plot

# Observed and predicted
y_obs <- df$Y
y_pred <- kf_result_2d$y_pred

# RMSE
mean_Y <- mean(y_obs)
rmse <- sqrt(mean((y_obs - y_pred)^2))
nrmse <- rmse / mean_Y

cat("RMSE: ", round(rmse, 3), "\n")
cat("Normalized RMSE: ", round(nrmse, 4), "\n")

compare_df <- data.frame(
  time      = df$time,
  observed  = df$Y,
  model_1d  = kf_result$y_pred,  # 1D model
  model_2d  = kf_result_2d$y_pred   # 2D model
)

# just 2d compared to observed values

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

# 1d vs 2d vs observed
library(tidyr)

compare_long <- compare_df %>%
  pivot_longer(cols = -time, names_to = "type", values_to = "value")
compare_long <- compare_long %>%
  mutate(type = factor(type, levels = c("observed", "model_1d", "model_2d")))

ggplot(compare_long, aes(x = time, y = value, color = type, linetype = type)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "observed" = "black",
      "model_1d" = "red",
      "model_2d" = "blue"
    ),
    labels = c("Observed", "1D Model", "2D Model")
  ) +
  scale_linetype_manual(
    values = c("observed" = "solid", "model_1d" = "dashed", "model_2d" = "dashed"),
    labels = c("Observed", "1D Model", "2D Model")
  ) +
  labs(
    title = "Observed vs. 1D and 2D Modelled Transformer Temperature",
    x = "Time (hours)",
    y = "Temperature (°C)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.043, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80")
  )


# Extract values
logLik <- -result_2d$value      # log-likelihood is the negative of value
k <- length(result_2d$par)      # number of parameters
n <- nrow(df)                   # number of observations

# Calculate
aic <- -2 * logLik + 2 * k
bic <- -2 * logLik + log(n) * k

# Print
cat("AIC:", round(aic, 2), "\n")
cat("BIC:", round(bic, 2), "\n")


######### 2.4 Interpret the two reconstructed states

library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare state data
state_df <- data.frame(
  time = df$time,
  state_1 = kf_result_2d$x_filtered[, 1],
  state_2 = kf_result_2d$x_filtered[, 2]
) %>%
  pivot_longer(cols = -time, names_to = "state", values_to = "value") %>%
  mutate(state = recode(state,
                        state_1 = "State 1 (x₁,t)",
                        state_2 = "State 2 (x₂,t)"
  ))

# Plot
ggplot(state_df, aes(x = time, y = value, color = state)) +
  geom_line(size = 1) +
  labs(
    title = "Reconstructed State Trajectories",
    x = "Time (hours)",
    y = "State Value",
    color = NULL
  ) +
  scale_color_manual(values = c("State 1 (x₁,t)" = "steelblue", "State 2 (x₂,t)" = "darkorange")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.04, 0.92),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80")
  )

# Extract states
x1 <- kf_result_2d$x_filtered[, 1]
x2 <- kf_result_2d$x_filtered[, 2]

# Time
t <- df$time

# Scale x2 to match x1 range
scale_factor <- diff(range(x1)) / diff(range(x2))
offset <- min(x1) - min(x2) * scale_factor
x2_scaled <- x2 * scale_factor + offset

# Plot
ggplot(data.frame(time = t, x1 = x1, x2_scaled = x2_scaled), aes(x = time)) +
  geom_line(aes(y = x1, color = "State 1 (x₁,t)"), size = 1) +
  geom_line(aes(y = x2_scaled, color = "State 2 (x₂,t)"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "State 1 (x₁,t)",  # left axis
    sec.axis = sec_axis(~ (. - offset) / scale_factor, name = "State 2 (x₂,t)")  # right axis
  ) +
  scale_color_manual(values = c("State 1 (x₁,t)" = "steelblue", "State 2 (x₂,t)" = "darkorange")) +
  labs(
    title = "Reconstructed State Trajectories (2D State-Space Model)",
    x = "Time (hours)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "darkorange"),
    legend.position = c(0.04, 0.92),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "gray80")
  )


# Extract parameter vector
params <- result_2d$par

# A matrix (2×2)
A_est <- matrix(params[1:4], nrow = 2, byrow = TRUE)

# B matrix (2×3) — 3 inputs: Ta, S, I
B_est <- matrix(params[5:10], nrow = 2, byrow = TRUE)

# Optionally give column names to B
colnames(B_est) <- c("Ta", "S", "I")
rownames(B_est) <- c("State 1 (x₁)", "State 2 (x₂)")

# View
A_est
B_est

# Load required packages and theme
library(tidyverse)
source("functions/plot_functions.R")
set_plot_style()

# Prepare combined data for states and inputs
plot_df <- data.frame(
  time = df$time,
  x1 = kf_result_2d$x_filtered[, 1],
  x2 = kf_result_2d$x_filtered[, 2],
  I  = df$I,
  Ta = df$Ta,
  S  = df$S
)

# Desired order for subplots
ordered_vars <- c("x1", "x2", "I", "Ta", "S")

# Convert variable to factor with specified order
plot_df_long$variable <- factor(
  plot_df_long$variable,
  levels = ordered_vars
)

variable_labels <- c(
  x1 = "x1",
  x2 = "x2",
  I  = "Transformer Load (kA)",
  Ta = "Outdoor Temp (°C)",
  S  = "Solar Radiation (W/m²)"
)

# Now re-use your ggplot code
ggplot(plot_df_long, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ variable, 
             scales = "free_y", 
             ncol = 1,
             labeller = as_labeller(variable_labels)) +
  scale_color_manual(values = variable_colors) +
  labs(
    title = "State Trajectories and Input Variables Over Time",
    x = "Time (hours)",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )

params <- result_2d$par

# Extract A matrix (2×2)
A_est <- matrix(result_2d$par[1:4], nrow = 2, byrow = TRUE)

# Print nicely with row/col names
colnames(A_est) <- c("x₁,t", "x₂,t")
rownames(A_est) <- c("x₁,t+1", "x₂,t+1")
print(round(A_est, 4))

# Extract B matrix (2×3)
B_est <- matrix(result_2d$par[5:10], nrow = 2, byrow = TRUE)

# Label inputs
colnames(B_est) <- c("Tₐ,t", "Φₛ,t", "Φᵢ,t")
rownames(B_est) <- c("x₁,t+1", "x₂,t+1")
print(round(B_est, 4))
