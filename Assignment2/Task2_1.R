# Load necessary libraries
library(forecast)

# Set seed for reproducibility
set.seed(123)

# Define ARIMA(1,0,0) × (0,0,0)[12] model with ϕ1 = 0.6
phi <- 0.6
n <- 200  # Number of observations

# Simulate AR(1) time series
sim_data <- arima.sim(model = list(ar = phi), n = n)

# Export the plot to a PNG file
png("ARIMA_Simulation.png", width = 1200, height = 800, res = 150)

# Define layout:
# - Row 1: Time series (full width)
# - Row 2: ACF (left), PACF (right)
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1,1))

# Plot Time Series (full width at the top)
plot.ts(sim_data, main = "Simulated ARIMA(1,0,0) × (0,0,0)[12]", 
        ylab = "Value", xlab = "Time", col = "black", lwd = 1.2)

# Plot ACF (left)
acf(sim_data, main = "Autocorrelation (ACF)", col = "black", lwd = 2)

# Plot PACF (right)
pacf(sim_data, main = "Partial Autocorrelation (PACF)", col = "brown", lwd = 2)