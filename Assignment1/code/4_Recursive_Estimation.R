#source("code/read_data.R")
source("Assignment1/code/read_data.R")
source("Assignment1/code/3_WLS_cleaned_2.R") 

library(plotly)  

y = Dtrain$total

X = cbind(1, Dtrain$year)

# Task 4.2

p <- ncol(X)
N <- nrow(X)
R <- diag(0.1, p)
theta <- c(0,0)
Theta <- matrix(NA, nrow=N, ncol=p)


# Iterate through and estimate the parameters
for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

# Task 4.3
Theta_OLS <- matrix(NA, nrow=N, ncol=p)
for(i in 1:N){
    m_OLS <- lm(total ~ year, data = Dtrain[1:i,])
    Theta_OLS[i, ] <- coef(m_OLS)
}

print(Theta_OLS[N,])
print(Theta[N,])

# Now checking for better initial values <3

theta = (Theta_OLS[N,])

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta_OLS[N,])
print(Theta[N,])

#Task 4.4

R <- diag(0.1, p)
theta <- c(0,0)
lambda <- 0.7

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- lambda * R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta[N,])
#Plot Theta[,1] and Theta[,2] against time
plot(Dtrain$year, Theta[,1], type = "l", col = "red", xlab = "Year", ylab = "Theta_1")
lines(Dtrain$year, Theta[,2], col = "blue")
legend("topright", legend = c("Theta_1", "Theta_2"), col = c("red", "blue"), lty = 1)
#Save the plot to file
#dev.copy(png, "plots/4.4-lambda-0.7.png")
dev.off()


R <- diag(0.1, p)
theta <- c(0,0)
lambda <- 0.99

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- lambda * R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta[N,])
plot(Dtrain$year, Theta[,1], type = "l", col = "red", xlab = "Year", ylab = "Theta_1")
lines(Dtrain$year, Theta[,2], col = "blue")
legend("topright", legend = c("Theta_1", "Theta_2"), col = c("red", "blue"), lty = 1)
#Save the plot to file
#dev.copy(png, "plots/4.4-lambda-0.99.png")
dev.off()


# 4.5 One step predictions
lambda_values <- c(0.7, 0.99)

# Initialize storage for predictions
OneStepPreds <- data.frame(Year = Dtrain$year)  # Store predictions per lambda
colors <- c("blue", "red")  # Colors for lambda curves

for (j in seq_along(lambda_values)) {
  lambda <- lambda_values[j]
  
  # Initialize R and Theta for RLS
  R <- diag(0.1, p)
  Theta <- matrix(NA, nrow=N, ncol=p)
  OneStepPred <- rep(NA, N)
  
  # First step: Initialize R and h
  x1 <- matrix(X[1, ], ncol=1)
  R <- (x1 %*% t(x1)) + diag(0.1, p)
  h <- x1 * y[1]
  
  # Iterate through data for RLS
  for(i in 2:N){
    x <- matrix(X[i, ], ncol=1)
    
    # Update R and h
    R <- lambda * R + x %*% t(x)
    h <- lambda * h + x * y[i]
    
    # Compute parameter estimates
    Theta[i, ] <- solve(R) %*% h
    
    # Compute one-step-ahead prediction (avoid out-of-bounds)
    if (i < N) {
      OneStepPred[i+1] <- X[i+1, ] %*% Theta[i, ]
    }
  }
  
  # Store predictions
  OneStepPreds[[paste0("Lambda_", lambda)]] <- OneStepPred
}

# Plot both lambda models
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="black", size=1.5) +  # Actual data
  geom_line(aes(y=OneStepPreds$Lambda_0.7, color="Lambda = 0.7"), size=1) +
  geom_line(aes(y=OneStepPreds$Lambda_0.99, color="Lambda = 0.99"), size=1) +
  labs(title = "One-Step Predictions Comparison (RLS)",
       x = "Year",
       y = "Total Vehicles",
       color = NULL) +  # Remove legend title
  scale_color_manual(values = colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = c(0.15, 0.85),
    legend.text = element_text(size = 12)
  )


# One-Step Prediction Errors

# Compute residuals (errors) using OneStepPreds
Error_07 <- Dtrain$total - OneStepPreds$Lambda_0.7
Error_99 <- Dtrain$total - OneStepPreds$Lambda_0.99

# Compute Mean Squared Error (MSE)
MSE_07 <- mean(Error_07^2, na.rm=TRUE)
MSE_99 <- mean(Error_99^2, na.rm=TRUE)

# Print MSE values
cat("MSE for λ = 0.7:", MSE_07, "\n")
cat("MSE for λ = 0.99:", MSE_99, "\n")

# Remove the first 4 values from error data
ErrorData <- data.frame(
  Year = Dtrain$year[-(1:4)],
  Error_07 = Error_07[-(1:4)],
  Error_99 = Error_99[-(1:4)]
)

ggplot(ErrorData, aes(x=Year)) +
  geom_point(aes(y=Error_07, color="Lambda = 0.7"), size=2) +  # Error points
  geom_line(aes(y=Error_07, color="Lambda = 0.7"), size=0.7) +  # Connect dots with line
  geom_point(aes(y=Error_99, color="Lambda = 0.99"), size=2) +  # Error points
  geom_line(aes(y=Error_99, color="Lambda = 0.99"), size=0.7) +  # Connect dots with line
  geom_smooth(aes(y=Error_07, color="Lambda = 0.7"), method="lm", se=FALSE, size=1.2, linetype="solid") +  # Linear trend line
  geom_smooth(aes(y=Error_99, color="Lambda = 0.99"), method="lm", se=FALSE, size=1.2, linetype="solid") +  # Linear trend line
  labs(title = "One-Step Prediction Errors with Linear Trend",
       x = "Year",
       y = "Prediction Error") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "topleft",  # Move legend to upper left
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12)
  )

# Task 4.6: Optimize Forgetting Factor (λ) Over Multiple Forecast Horizons (k)
lambda_values <- seq(0.5, 0.99, by=0.01)  # Test values from 0.5 to 0.99
horizons <- 1:12  # Forecast horizons k = 1 to 12

RMSE_results <- expand.grid(Lambda = lambda_values, Horizon = horizons)
RMSE_results$RMSE <- NA  # Placeholder for RMSE values

X <- cbind(1, Dtrain$year)
y <- cbind(Dtrain$total)
N <- nrow(X) 

# Loop through each lambda value
for (j in seq_along(lambda_values)) {
  lambda <- lambda_values[j]
  
  # Initialize R and Theta for RLS
  R <- diag(0.1, p)
  Theta <- matrix(NA, nrow=N, ncol=p)
  Y_pred <- matrix(NA, nrow=N, ncol=max(horizons))  # Store k-step ahead forecasts
  
  # First step: Initialize R and h
  x1 <- matrix(X[1, ], ncol=1)
  R <- (x1 %*% t(x1)) + diag(0.1, p)
  h <- x1 * y[1]
  
  # Iterate through data for RLS
  for(i in 2:N){
    x <- matrix(X[i, ], ncol=1)
    
    # Update R and h
    R <- lambda * R + x %*% t(x)
    h <- lambda * h + x * y[i]
    
    # Compute parameter estimates
    Theta[i, ] <- solve(R) %*% h
    
    # Compute k-step ahead predictions
    for (k in horizons) {
      if (i + k <= N) {  # Ensure within bounds
        Y_pred[i, k] <- X[i + k, ] %*% Theta[i, ]
      }
    }
  }
  
  # Compute RMSE for each horizon k
  for (k in horizons) {
    Errors <- Y_pred[(k + 1):N, k] - y[(k + 1):N]
    RMSE_results$RMSE[RMSE_results$Lambda == lambda & RMSE_results$Horizon == k] <- sqrt(mean(Errors^2, na.rm=TRUE))
  }
}

ggplot(RMSE_results, aes(x=Horizon, y=RMSE, color=factor(Lambda), group=Lambda)) +
  geom_line(size=1) +
  labs(title = "RMSE vs. Forecast Horizon (k) for Different Forgetting Factors (λ)",
       x = "Forecast Horizon (k)",
       y = "Root Mean Squared Error (RMSE)",
       color = "Forgetting Factor (λ)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )

# Convert RMSE results into a matrix format for 3D plotting
RMSE_matrix <- reshape2::acast(RMSE_results, Horizon ~ Lambda, value.var="RMSE")

# Generate 3D Surface Plot
plot_ly(x = lambda_values, 
        y = horizons, 
        z = RMSE_matrix, 
        type = "surface",
        colorscale = "Viridis") %>%
  layout(title = "3D Surface Plot: RMSE vs. Horizon (k) and Forgetting Factor (λ)",
         scene = list(
           xaxis = list(title = "Forgetting Factor (λ)"),
           yaxis = list(title = "Forecast Horizon (k)"),
           zaxis = list(title = "Root Mean Squared Error (RMSE)")
         ))

# Task 4.7: Predicting Test Set Using RLS
lambda <- 0.77  # Chosen forgetting factor for RLS
horizon <- 12  # Forecast horizon

# Use the training dataset for parameter estimation
X_train <- cbind(1, Dtrain$year)
y_train <- cbind(Dtrain$total)
N_train <- nrow(X_train)

# Use the test dataset for predictions
X_test <- cbind(1, Dtest$year)
y_test <- cbind(Dtest$total)
N_test <- nrow(X_test)

## Compute RLS Predictions
R <- diag(0.1, p)
Theta <- matrix(NA, nrow=N_train, ncol=p)  # Store parameter estimates
Y_pred_rls <- rep(NA, N_test)  # Store RLS test predictions

# First step: Initialize R and h
x1 <- matrix(X_train[1, ], ncol=1)
R <- (x1 %*% t(x1)) + diag(0.1, p)
h <- x1 * y_train[1]

# Run RLS on the training data
for (i in 2:N_train) {
  x <- matrix(X_train[i, ], ncol=1)
  
  # Update R and h
  R <- lambda * R + x %*% t(x)
  h <- lambda * h + x * y_train[i]
  
  # Compute parameter estimates
  Theta[i, ] <- solve(R) %*% h
}

# Use final estimated parameters from training to predict test set
Theta_final <- Theta[N_train, ]

for (i in 1:N_test) {
  # Predict using the most recent estimated parameters
  Y_pred_rls[i] <- X_test[i, ] %*% Theta_final
}


## Compute Corrected RLS Prediction Intervals
# Compute residuals from recent history (last H observations)
H_effective <- 1 / (1 - lambda)  # Effective horizon
recent_residuals <- y_train[(N_train - floor(H_effective) + 1):N_train] - 
  X_train[(N_train - floor(H_effective) + 1):N_train, ] %*% Theta_final

# Estimate variance from only the most recent residuals
sigma2_rls <- sum(recent_residuals^2) / (length(recent_residuals) - p)  # More stable variance

# Compute standard errors for test predictions
Vmatrix_rls <- sigma2_rls * (1 + diag(X_test %*% solve(tail(R, p)) %*% t(X_test)))  # Use last R
se_rls <- sqrt(Vmatrix_rls)

# Compute 95% prediction intervals
alpha <- 0.05
t_value <- qt(1 - alpha/2, df=length(recent_residuals)-p)
RLS_Lower_PI <- Y_pred_rls - t_value * se_rls
RLS_Upper_PI <- Y_pred_rls + t_value * se_rls

## OLS and WLS
Y_pred_ols <- y_pred_ols[1:N_test]  # Match OLS predictions with test period
Y_pred_wls <- y_pred_wls[1:N_test]  # Match WLS predictions with test period

# Extract WLS prediction intervals for test set
WLS_Lower_PI <- forecast_data$Lower_PI[forecast_data$Time %in% Dtest$year]
WLS_Upper_PI <- forecast_data$Upper_PI[forecast_data$Time %in% Dtest$year]

# Extract OLS prediction intervals for test set
OLS_Lower_PI <- forecast_data$OLS_Lower_PI[forecast_data$Time %in% Dtest$year]
OLS_Upper_PI <- forecast_data$OLS_Upper_PI[forecast_data$Time %in% Dtest$year]

ggplot() +
  geom_point(data=Dtrain, aes(x=year, y=total, color="Training Data"), size=2) +
  geom_line(data=Dtrain, aes(x=year, y=total, color="Training Data")) +
  geom_point(data=Dtest, aes(x=year, y=total, color="Test Data"), size=2) +
  geom_line(data=Dtest, aes(x=year, y=total, color="Test Data")) +
  
  # RLS Predictions
  geom_point(aes(x=Dtest$year, y=Y_pred_rls, color="RLS Prediction (k = 12, λ = 0.77)"), size=2) +
  geom_line(aes(x=Dtest$year, y=Y_pred_rls, color="RLS Prediction (k = 12, λ = 0.77)")) +
  
  # OLS Predictions
  geom_point(aes(x=Dtest$year, y=Y_pred_ols, color="OLS Prediction"), size=2) +
  geom_line(aes(x=Dtest$year, y=Y_pred_ols, color="OLS Prediction")) +
  
  # WLS Predictions
  geom_point(aes(x=Dtest$year, y=Y_pred_wls, color="WLS Prediction"), size=2) +
  geom_line(aes(x=Dtest$year, y=Y_pred_wls, color="WLS Prediction")) +
  
  # OLS Prediction Interval
  geom_ribbon(aes(x=Dtest$year, ymin=OLS_Lower_PI, ymax=OLS_Upper_PI), 
              fill="green", alpha=0.2, show.legend=FALSE) +
  
  # WLS Prediction Interval (Enhanced Visibility)
  geom_ribbon(aes(x=Dtest$year, ymin=WLS_Lower_PI, ymax=WLS_Upper_PI), 
              fill="orange", alpha=0.4, show.legend=FALSE) +
  
  # RLS Prediction Interval
  geom_ribbon(aes(x=Dtest$year, ymin=RLS_Lower_PI, ymax=RLS_Upper_PI), 
              fill="blue", alpha=0.2, show.legend=FALSE) +
  
  # Labels and Legend
  labs(title = "RLS, WLS, and OLS Forecasts for Test Set",
       x = "Year",
       y = "Total Vehicles (millions)",
       color = NULL) +
  
  # Custom Colors
  scale_color_manual(values = c(
    "Training Data" = "blue",
    "Test Data" = "red",
    "RLS Prediction (k = 12, λ = 0.77)" = "purple",
    "OLS Prediction" = "green",
    "WLS Prediction" = "orange"
  )) +
  
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )