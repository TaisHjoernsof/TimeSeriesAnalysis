# Load required libraries
library(dplyr)

# Load data (adjust path if needed)
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Split into train/test
train_data <- data %>% filter(Time <= 167)

# Fit ARX(3) model
arx3_model <- lm(Ph ~ Ph.l1 + Ph.l2 + Ph.l3 +
                   Tdelta + Tdelta.l1 + Tdelta.l2 +
                   Gv + Gv.l1 + Gv.l2,
                 data = train_data)

# Print model summary
summary(arx3_model)

# Extract coefficients
params <- coef(arx3_model)

# Store each coefficient in a variable for simulation
intercept    <- params["(Intercept)"]
phi1         <- params["Ph.l1"]
phi2         <- params["Ph.l2"]
phi3         <- params["Ph.l3"]
omega_T_0    <- params["Tdelta"]
omega_T_1    <- params["Tdelta.l1"]
omega_T_2    <- params["Tdelta.l2"]
omega_G_0    <- params["Gv"]
omega_G_1    <- params["Gv.l1"]
omega_G_2    <- params["Gv.l2"]

# Set up vector to store simulated predictions
N <- nrow(data)
Ph_sim <- rep(NA, N)

# Initial conditions — use actual Ph values for t = 1 to 3
Ph_sim[1:3] <- data$Ph[1:3]

# Simulate from t = 4 to N (both train + test period)
for (t in 4:N) {
  Ph_sim[t] <- intercept +
    phi1 * Ph_sim[t - 1] +
    phi2 * Ph_sim[t - 2] +
    phi3 * Ph_sim[t - 3] +
    omega_T_0 * data$Tdelta[t] +
    omega_T_1 * data$Tdelta[t - 1] +
    omega_T_2 * data$Tdelta[t - 2] +
    omega_G_0 * data$Gv[t] +
    omega_G_1 * data$Gv[t - 1] +
    omega_G_2 * data$Gv[t - 2]
}

# Add to dataframe
data$Ph_sim <- Ph_sim


# Define plot style
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.175, 0.90),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Plot actual vs simulated Ph
ggplot(data, aes(x = Time)) +
  geom_line(aes(y = Ph, color = "Actual")) +
  geom_line(aes(y = Ph_sim, color = "Simulated")) +
  scale_color_manual(values = c("Actual" = "red", "Simulated" = "black")) +
  labs(
    title = "Multi-step Prediction (Simulation) of Heater Power (ARX(3) Model)",
    x = "Time",
    y = "Heater Power (W)"
  ) +
  tight_theme

# Step 1: One-step prediction using fitted model
data$Ph_hat_arx3 <- predict(arx3_model, newdata = data)

# Step 2: Residuals
data$resid_onestep <- data$Ph - data$Ph_hat_arx3
data$resid_multistep <- data$Ph - data$Ph_sim

# Step 3: Cumulative sum of absolute residuals (could also do squared)
data$cumresid_onestep <- cumsum(abs(data$resid_onestep))
data$cumresid_multistep <- cumsum(abs(data$resid_multistep))

# Step 4: Plot in same visual style
library(ggplot2)

tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.23, 0.85),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Step 5: Plot cumulative residuals
ggplot(data, aes(x = Time)) +
  geom_line(aes(y = cumresid_onestep, color = "One-step")) +
  geom_line(aes(y = cumresid_multistep, color = "Multi-step")) +
  scale_color_manual(values = c("One-step" = "blue", "Multi-step" = "black")) +
  labs(
    title = "Cumulative Absolute Residuals: One-step vs. Multi-step Prediction (ARX(3))",
    y = "Cumulative Residual",
    x = "Time"
  ) +
  tight_theme

# ========================
# Compute RMSE, nRMSE, and Cumulative Residuals for One-step and Multi-step
# ========================

# Define test set index
test_rows <- which(data$Time > 167)

# Extract actuals and predictions
actual <- data$Ph
actual_test <- data$Ph[test_rows]

pred_onestep <- data$Ph_hat_arx3
pred_onestep_test <- pred_onestep[test_rows]

pred_multistep <- data$Ph_sim
pred_multistep_test <- pred_multistep[test_rows]

# Compute residuals
resid_onestep <- actual - pred_onestep
resid_multistep <- actual - pred_multistep

resid_onestep_test <- actual_test - pred_onestep_test
resid_multistep_test <- actual_test - pred_multistep_test

# RMSE (full and test set)
rmse_onestep_total <- sqrt(mean(resid_onestep^2, na.rm = TRUE))
rmse_multistep_total <- sqrt(mean(resid_multistep^2, na.rm = TRUE))

rmse_onestep_test <- sqrt(mean(resid_onestep_test^2, na.rm = TRUE))
rmse_multistep_test <- sqrt(mean(resid_multistep_test^2, na.rm = TRUE))

# Cumulative residuals (absolute)
cumresid_onestep_total <- sum(abs(resid_onestep), na.rm = TRUE)
cumresid_multistep_total <- sum(abs(resid_multistep), na.rm = TRUE)

cumresid_onestep_test <- sum(abs(resid_onestep_test), na.rm = TRUE)
cumresid_multistep_test <- sum(abs(resid_multistep_test), na.rm = TRUE)

# Normalization factors (test set only)
range_test <- max(actual_test, na.rm = TRUE) - min(actual_test, na.rm = TRUE)
mean_test <- mean(actual_test, na.rm = TRUE)

# Normalized RMSE (%)
nrmse_onestep_range_pct <- 100 * rmse_onestep_test / range_test
nrmse_multistep_range_pct <- 100 * rmse_multistep_test / range_test

nrmse_onestep_mean_pct <- 100 * rmse_onestep_test / mean_test
nrmse_multistep_mean_pct <- 100 * rmse_multistep_test / mean_test

# ========================
# Print Summary to Console
# ========================

cat("========= Model Performance Summary (ARX(3)) =========\n")
cat("\n--- Root Mean Square Error (RMSE) ---\n")
cat(sprintf("One-step   (full period): %.3f\n", rmse_onestep_total))
cat(sprintf("Multi-step (full period): %.3f\n", rmse_multistep_total))
cat(sprintf("One-step   (test set)   : %.3f\n", rmse_onestep_test))
cat(sprintf("Multi-step (test set)   : %.3f\n", rmse_multistep_test))

cat("\n--- Cumulative Absolute Residuals ---\n")
cat(sprintf("One-step   (full period): %.2f\n", cumresid_onestep_total))
cat(sprintf("Multi-step (full period): %.2f\n", cumresid_multistep_total))
cat(sprintf("One-step   (test set)   : %.2f\n", cumresid_onestep_test))
cat(sprintf("Multi-step (test set)   : %.2f\n", cumresid_multistep_test))

cat("\n--- Normalized RMSE (% on Test Set) ---\n")
cat(sprintf("By Range - One-step:   %.2f%%\n", nrmse_onestep_range_pct))
cat(sprintf("By Range - Multi-step: %.2f%%\n", nrmse_multistep_range_pct))
cat(sprintf("By Mean  - One-step:   %.2f%%\n", nrmse_onestep_mean_pct))
cat(sprintf("By Mean  - Multi-step: %.2f%%\n", nrmse_multistep_mean_pct))



