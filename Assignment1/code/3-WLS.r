# Load required packages
library(xtable)
library(ggplot2)

# Source the data processing script
source("Assignment 1/code/read_data.R")

## Step 1 - get OLS variance-covariance matrix from task 2

# Select first 3 observations
y <- Dtrain$total[1:3]
x <- Dtrain$year[1:3]

# Fit OLS model
m <- lm(y ~ x)

# Extract Variance-Covariance Matrix for OLS
V_OLS <- vcov(m)

# Print OLS Variance-Covariance Matrix
cat("\nOLS Variance-Covariance Matrix:\n")
print(V_OLS)

# Compute standard errors
se_OLS <- sqrt(diag(V_OLS))
cat("\nOLS Standard Errors:\nIntercept:", se_OLS[1], "\nSlope:", se_OLS[2], "\n")

## Step 2 - Define WLS model, calculate variance covariance matrix

# Define response variable and predictor
y <- matrix(Dtrain$total, ncol = 1)  # Full response variable
x <- Dtrain$year  # Full predictor variable
X <- cbind(1, x)  # Design matrix (Intercept + Year)
n <- nrow(Dtrain)  # Number of training observations

# Define forgetting factor
lambda <- 0.9  

# Compute weights for training data only
weights <- lambda^((n - 1):0)  

# Construct weight matrix W
W <- diag(weights)

# Compute inverse of W (needed for covariance matrix of residuals)
W_inv <- diag(1 / weights)

# Estimate variance (sigma^2 for residuals)
yhat_wls <- X %*% solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
residuals_wls <- y - yhat_wls
RSS_wls <- t(residuals_wls) %*% W %*% residuals_wls
sigma2_wls <- as.numeric(RSS_wls / (n - 2))  # Using n - p degrees of freedom

# Compute the NxN variance-covariance matrix of residuals
Sigma_NxN <- sigma2_wls * W_inv

# Extract diagonal elements
residual_variance <- diag(Sigma_NxN)

# Open a new graphics window (useful in some environments)
dev.new()

# Plot variance over time
plot(1:n, residual_variance, type = "o", col = "blue", 
     main = "Residual Variance Over Training Period", 
     xlab = "Observation Index", ylab = "Variance")


# Print a small portion of Sigma_NxN
cat("\nNxN Variance-Covariance Matrix (First 5x5 Elements):\n")
print(Sigma_NxN[1:5, 1:5])
cat("\nNxN Variance-Covariance Matrix (Last 6x6 Elements):\n")
print(Sigma_NxN[(n-5):n, (n-5):n])

# Compute WLS Variance-Covariance Matrix for Parameter Estimates (2x2)
V_WLS <- sigma2_wls * solve(t(X) %*% W %*% X)

# Print the 2x2 matrix
cat("\n2x2 Variance-Covariance Matrix of Parameter Estimates:\n")
print(V_WLS)


## Task 3.2 - Plot λ-weights (for training data only)

# Create a dataframe for plotting

weight_data <- data.frame(
  Time = Dtrain$year,
  Weight = weights
)

# Extract unique years and append "2024 (end)"
unique_years <- unique(floor(weight_data$Time))  # Get distinct year values
unique_years <- c(unique_years, 2024)  # Append 2024 as the final year

# Define labels with "(end)" for 2024
year_labels <- as.character(unique_years)
year_labels[length(year_labels)] <- "2024"  # Rename last year

# Plot weights over time, displaying only full years
ggplot(weight_data, aes(x = Time, y = Weight)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  geom_point(color = "red", size = 2) +  # Points for each observation
  labs(title = "λ-Weights Over Time",
       x = "Year",
       y = "Weight (λ)") +
  scale_x_continuous(
    breaks = unique_years,  # Show only full years as tick marks
    labels = year_labels  # Label years, with "2024 (end)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),  # Center title
    axis.title = element_text(size = 24),  # Increase axis title size
    axis.text = element_text(size = 20),  # Increase tick label size
    legend.position = "none"  # No legend needed
  )


## Task 3.3 sum lambda weights
# Compute sum of λ-weights for WLS
sum_lambda_weights <- sum(weights)

# Compute sum of weights for OLS (which is just the number of observations)
sum_OLS_weights <- length(weights)

# Print results
cat("Sum of WLS λ-weights:", sum_lambda_weights, "\n")
cat("Sum of OLS weights:", sum_OLS_weights, "\n")



## Task 3.4 Compute WLS Parameter Estimates
# Compute WLS parameter estimates (θ̂₁ and θ̂₂)
theta_WLS <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)

# Extract parameter values
theta_1 <- theta_WLS[1]
theta_2 <- theta_WLS[2]

# Print estimates
cat("Estimated WLS Parameters:\n")
cat("θ̂₁ (Intercept):", theta_1, "\n")
cat("θ̂₂ (Slope):", theta_2, "\n") 

# Compute residuals
yhat_wls <- X %*% theta_WLS  # Predicted values
residuals_wls <- y - yhat_wls  # Residuals

# Compute variance estimate (sigma²)
RSS_wls <- t(residuals_wls) %*% W %*% residuals_wls
sigma2_wls <- as.numeric(RSS_wls / (n - 2))  # Degrees of freedom = n - 2

# Compute variance-covariance matrix for parameter estimates
V_WLS <- sigma2_wls * solve(t(X) %*% W %*% X)

# Compute standard errors
se_theta_1 <- sqrt(V_WLS[1, 1])  # Standard error of intercept
se_theta_2 <- sqrt(V_WLS[2, 2])  # Standard error of slope

# Print results
cat("\nStandard Errors of WLS Estimates:\n")
cat("SE(θ̂₁):", se_theta_1, "\n")
cat("SE(θ̂₂):", se_theta_2, "\n")


## Task 3.5 Forecasts
# Extract the exact time index from Dtest for forecasting
future_years <- Dtest$year  # Use test data timestamps directly

# Create new design matrix for future values
X_future <- cbind(1, future_years)

# Compute WLS predictions for test set timestamps
y_pred_wls <- X_future %*% theta_WLS  # Forecasted values

# Store results in a dataframe
forecast_data <- data.frame(
  Time = future_years,
  Forecast = y_pred_wls
)

# Compute standard errors for predictions
Vmatrix_pred <- sigma2_wls * (1 + (X_future %*% solve(t(X) %*% W %*% X) %*% t(X_future)))
se_pred <- sqrt(diag(Vmatrix_pred))

# Compute 95% prediction intervals
alpha <- 0.05
t_value <- qt(1 - alpha/2, df = n-2)  # t-distribution critical value

# Upper and lower bounds
forecast_data$Lower_PI <- forecast_data$Forecast - t_value * se_pred
forecast_data$Upper_PI <- forecast_data$Forecast + t_value * se_pred

# Print forecast with prediction intervals
print(forecast_data)
ggplot() +
  # Plot training data
  geom_point(data = Dtrain, aes(x = year, y = total), color = "blue") +
  geom_line(data = Dtrain, aes(x = year, y = total), color = "blue") +
  
  # Plot test data
  geom_point(data = Dtest, aes(x = year, y = total), color = "black") +
  
  # Plot WLS forecast (matching test set index)
  geom_point(data = forecast_data, aes(x = Time, y = Forecast), color = "red") +
  geom_line(data = forecast_data, aes(x = Time, y = Forecast), color = "red") +
  
  # Add prediction intervals as shaded region
  geom_ribbon(data = forecast_data, aes(x = Time, ymin = Lower_PI, ymax = Upper_PI), 
              fill = "red", alpha = 0.2) +
  
  labs(title = "WLS Forecast for 2024",
       x = "Year",
       y = "Total Vehicles (millions)") +
  
  scale_x_continuous(breaks = seq(2018, 2024, by = 1)) +  # Show yearly breaks
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.position = "none"
  )






