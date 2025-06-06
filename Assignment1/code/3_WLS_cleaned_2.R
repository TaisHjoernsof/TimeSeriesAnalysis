# Load required packages
library(xtable)
library(ggplot2)

# Source the data processing script
source("code/read_data.R")

# ==============================
# Model Definitions
# ==============================

### OLS
# Fit OLS model
m_OLS <- lm(total ~ year, data = Dtrain)
V_OLS <- vcov(m_OLS)  # Variance-Covariance Matrix
se_OLS <- sqrt(diag(V_OLS))  # Standard errors

# OLS Predictions with standard error intervals
ols_pred <- predict(m_OLS, newdata = data.frame(year = Dtest$year), interval = "prediction", level = 0.95)

y_pred_ols <- ols_pred[, "fit"]
y_pred_ols_lower <- ols_pred[, "lwr"]
y_pred_ols_upper <- ols_pred[, "upr"]

### WLS

# Define response variable and predictor for full dataset
y <- matrix(Dtrain$total, ncol = 1)  # Full response variable
x <- Dtrain$year  # Full predictor variable
X <- cbind(1, x)  # Design matrix (Intercept + Year)
n <- nrow(Dtrain)  # Number of training observations

# Define forgetting factor for WLS
lambda <- 0.9  
weights <- lambda^((n - 1):0)  # Compute weights
W <- diag(weights)  # Construct weight matrix W
W_inv <- diag(1 / weights)  # Compute inverse of W

# Compute WLS parameter estimates
theta_WLS <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
yhat_wls <- X %*% theta_WLS  # Predicted values
residuals_wls <- y - yhat_wls  # Residuals

# Estimate variance sigma²
RSS_wls <- t(residuals_wls) %*% W %*% residuals_wls
sigma2_wls <- as.numeric(RSS_wls / (n - 2))

# Compute variance-covariance matrix
V_WLS <- sigma2_wls * solve(t(X) %*% W %*% X)
se_WLS <- sqrt(diag(V_WLS))

# ==============================
# Task 3.1: Variance-Covariance Matrix
# ==============================

cat("\nOLS Variance-Covariance Matrix:\n")
print(V_OLS)
cat("\nOLS Standard Errors:\nIntercept:", se_OLS[1], "\nSlope:", se_OLS[2], "\n")

cat("\nWLS Variance-Covariance Matrix:\n")
print(V_WLS)
cat("\nWLS Standard Errors:\nIntercept:", se_WLS[1], "\nSlope:", se_WLS[2], "\n")

# Compute full residual variance-covariance matrix for WLS
Sigma_WLS <- sigma2_wls * W_inv

# Compute OLS residual variance-covariance matrix (Identity Matrix)
Sigma_OLS <- sigma2_wls * diag(n)

# Print relevant parts
cat("\nFirst 5×5 Elements of OLS Variance-Covariance Matrix:\n")
print(Sigma_OLS[1:5, 1:5])

cat("\nLast 5×5 Elements of OLS Variance-Covariance Matrix:\n")
print(Sigma_OLS[(n-4):n, (n-4):n])

# Print only relevant parts of the 72×72 matrix
cat("\nFirst 5×5 Elements of WLS Variance-Covariance Matrix:\n")
print(Sigma_WLS[1:5, 1:5])

cat("\nLast 5×5 Elements of WLS Variance-Covariance Matrix:\n")
print(Sigma_WLS[(n-4):n, (n-4):n])


# ==============================
# Task 3.2: λ-Weights Analysis
# ==============================

# Create dataframe for visualization
weight_data <- data.frame(Time = Dtrain$year, Weight = weights)

ggplot(weight_data, aes(x = Time, y = Weight)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Weights Over Time", x = "Year", y = "Weight") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none"
  )

# ==============================
# Task 3.3: λ-Weights Sum
# ==============================

# Compute sum of λ-weights for WLS
sum_lambda_weights <- sum(weights)

# Compute sum of weights for OLS (which is just the number of observations)
sum_OLS_weights <- length(weights)

# Print results
cat("Sum of WLS λ-weights:", sum_lambda_weights, "\n")
cat("Sum of OLS weights:", sum_OLS_weights, "\n")

# ==============================
# Task 3.4: Estimate WLS (and OLS) model parameters
# ==============================

cat("\nEstimated WLS Parameters:\n")
cat("Intercept (θ̂₁):", theta_WLS[1], "\nSlope (θ̂₂):", theta_WLS[2], "\n")

cat("\nWLS Standard Errors:\n")
cat("SE(θ̂₁):", se_WLS[1], "\nSE(θ̂₂):", se_WLS[2], "\n")

# Extract estimated OLS parameters
theta_OLS <- coef(m_OLS)  # Intercept and slope
cat("\nEstimated OLS Parameters:\n")
cat("Intercept (θ̂₁):", theta_OLS[1], "\nSlope (θ̂₂):", theta_OLS[2], "\n")

cat("\nOLS Standard Errors:\n")
cat("SE(θ̂₁):", se_OLS[1], "\nSE(θ̂₂):", se_OLS[2], "\n")

# ==============================
# Task 3.5: Forecasting
# ==============================

# Define test period
time_future <- Dtest$year  # Extract future time indices
X_future <- cbind(1, time_future)  # Design matrix for test set

y_pred_wls <- X_future %*% theta_WLS  # Forecasted values

# Compute prediction standard errors
Vmatrix_pred <- sigma2_wls * (1 + (X_future %*% solve(t(X) %*% W %*% X) %*% t(X_future)))
se_pred <- sqrt(diag(Vmatrix_pred))

# Compute residuals for WLS and OLS
residuals_wls <- Dtest$total - y_pred_wls
residuals_ols <- Dtest$total - y_pred_ols

# Compute MSE for WLS and OLS
mse_wls <- mean(residuals_wls^2)
mse_ols <- mean(residuals_ols^2)

# Compute RMSE for WLS and OLS
rmse_wls <- sqrt(mse_wls)
rmse_ols <- sqrt(mse_ols)

# Print results
cat("MSE (WLS):", mse_wls, "\nMSE (OLS):", mse_ols, "\n")
cat("RMSE (WLS):", rmse_wls, "\nRMSE (OLS):", rmse_ols, "\n")


# Compute 95% prediction intervals
alpha <- 0.05
t_value <- qt(1 - alpha/2, df = n-2)
forecast_data <- data.frame(
  Time = time_future,
  Forecast = y_pred_wls,
  Lower_PI = y_pred_wls - t_value * se_pred,
  Upper_PI = y_pred_wls + t_value * se_pred,
  OLS_Prediction = y_pred_ols,
  OLS_Lower_PI = y_pred_ols_lower,
  OLS_Upper_PI = y_pred_ols_upper
)

# Plot forecast
ggplot(forecast_data, aes(x = Time)) +
  geom_point(data = Dtrain, aes(x = year, y = total, color = "Training Data"), size = 2) +
  geom_line(data = Dtrain, aes(x = year, y = total, color = "Training Data")) +
  geom_point(data = Dtest, aes(x = year, y = total, color = "Test Data"), size = 2) +
  geom_line(data = Dtest, aes(x = year, y = total, color = "Test Data")) +
  geom_point(aes(y = Forecast, color = "WLS Prediction"), size = 2) +
  geom_point(aes(y = OLS_Prediction, color = "OLS Prediction"), size = 2) +
  geom_ribbon(aes(ymin = forecast_data$Lower_PI, ymax = forecast_data$Upper_PI), fill = "purple", alpha = 0.2, show.legend = FALSE) +
  geom_ribbon(aes(ymin = OLS_Lower_PI, ymax = OLS_Upper_PI), fill = "green", alpha = 0.2, show.legend = FALSE) +
  scale_color_manual(values = c("Training Data" = "blue", "Test Data" = "red", "WLS Prediction" = "purple", "OLS Prediction" = "green")) +
  labs(title = "WLS & OLS Forecast for 2024", x = "Year", y = "Total Vehicles (millions)", color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = c(0.75, 0.313),
    legend.text = element_text(size = 14)
  )

forecast_data$Lower_PI