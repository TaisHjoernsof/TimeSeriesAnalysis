# Load required packages
library(xtable)
library(ggplot2)

# Source the data processing script
source("Assignment1/code/read_data.R")

# ==============================
# Model Definitions
# ==============================

### OLS
# Fit OLS model
m_OLS <- lm(total ~ year, data = Dtrain)
V_OLS <- vcov(m_OLS)  # Variance-Covariance Matrix
se_OLS <- sqrt(diag(V_OLS))  # Standard errors

# OLS Predictions with standard error intervals
ols_pred <- predict(m_OLS, newdata = data.frame(year = time_future), interval = "prediction", level = 0.95)

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
# Task 3.4: Estimate WLS model parameters
# ==============================

cat("\nEstimated WLS Parameters:\n")
cat("Intercept (θ̂₁):", theta_WLS[1], "\nSlope (θ̂₂):", theta_WLS[2], "\n")

cat("\nWLS Standard Errors:\n")
cat("SE(θ̂₁):", se_WLS[1], "\nSE(θ̂₂):", se_WLS[2], "\n")

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
  geom_ribbon(aes(ymin = Lower_PI, ymax = Upper_PI), fill = "purple", alpha = 0.2, show.legend = FALSE) +
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

# ==============================
# Task 3.6: Parameter Estimation for multiple lambda values
# ==============================
lambda_values <- c(0.99, 0.8, 0.7, 0.6)
forecast_results <- list()
forecast_data <- data.frame()

for (lambda in lambda_values) {
  # Compute weights for the given λ
  weights <- lambda^((n - 1):0)
  W <- diag(weights)
  
  # Estimate WLS parameters
  theta_WLS <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
  yhat_wls <- X %*% theta_WLS  # Predicted values
  residuals_wls <- y - yhat_wls  # Residuals
  
  # Estimate variance sigma²
  RSS_wls <- t(residuals_wls) %*% W %*% residuals_wls
  sigma2_wls <- as.numeric(RSS_wls / (n - 2))
  
  # Compute variance-covariance matrix
  V_WLS <- sigma2_wls * solve(t(X) %*% W %*% X)
  se_WLS <- sqrt(diag(V_WLS))
  
  # Compute forecasts for the next 12 months
  X_future <- cbind(1, time_future)
  y_pred_wls <- X_future %*% theta_WLS
  
  # Store results
  forecast_results[[as.character(lambda)]] <- list(
    lambda = lambda,
    theta_WLS = theta_WLS,
    sigma2_WLS = sigma2_wls,
    V_WLS = V_WLS,
    se_WLS = se_WLS,
    y_pred_wls = y_pred_wls
  )
  
  # Collect forecast data for plotting
  forecast_data <- rbind(forecast_data, 
                         data.frame(Time = time_future, Forecast = y_pred_wls, Lambda = as.factor(lambda)))
  
  # Print results
  cat("\nLambda:", lambda, "\n")
  cat("Intercept (θ̂₁):", theta_WLS[1], "\nSlope (θ̂₂):", theta_WLS[2], "\n")
  cat("Variance estimate (sigma²):", sigma2_wls, "\n")
}

# Compare how λ affects slope estimates
lambda_slopes <- sapply(forecast_results, function(res) res$theta_WLS[2])
lambda_slopes_df <- data.frame(Lambda = lambda_values, Slope = lambda_slopes)

# Print slope estimates for different λ
print(lambda_slopes_df)

# Plot effect of λ on forecasts
ggplot() +
  geom_point(data = Dtest, aes(x = year, y = total, color = "Test Data"), size = 2) +
  geom_line(data = forecast_data, aes(x = Time, y = Forecast, color = Lambda), size = 1) +
  scale_color_manual(values = c("0.99" = "purple", "0.8" = "blue", "0.7" = "green", "0.6" = "red", "Test Data" = "black")) +
  labs(title = "Effect of λ on Forecasts",
       x = "Year", y = "Total Vehicles",
       color = "λ Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = c(0.8, 0.6),
    legend.text = element_text(size = 14),
  )

