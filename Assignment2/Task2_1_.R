# Load necessary library
library(forecast)

# Reset graphics
graphics.off()
dev.new()

# Set model parameters
p <- 1  # AR order
d <- 0  # Integration order (no differencing)
q <- 0  # MA order

# Seasonal part (not used in this case)
P <- 0
D <- 0
Q <- 0
Period <- 12  # Seasonal period (not used here)

# Define fixed AR(1) coefficient
phi <- 0.6  # AR(1) coefficient

# Simulate ARIMA process
n <- 200  # Number of observations
sim_data <- arima.sim(model = list(order = c(p,d,q), ar = phi), n = n)

# Define layout: Time series on top, ACF & PACF below side by side
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))

# Plot Time Series
plot.ts(sim_data, main="Simulated ARIMA(1,0,0) × (0,0,0)[12] with ϕ₁ = 0.6", 
        ylab="Value", xlab="Time", col="black", lwd=1.2)

# Plot ACF (left)
acf(sim_data, main="Autocorrelation (ACF)", col="black", lwd=2)

# Plot PACF (right)
pacf(sim_data, main="Partial Autocorrelation (PACF)", col="brown", lwd=2)

# Reset layout
par(mfrow=c(1,1))

# Now save as PNG
png("Task_2_1_ARIMA_1_0_0.png", width = 1200, height = 800, res = 150)

# Redraw the plots for saving
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))
plot.ts(sim_data, main="Simulated ARIMA(1,0,0) × (0,0,0)[12] with ϕ₁ = 0.6", 
        ylab="Value", xlab="Time", col="black", lwd=1.2)
acf(sim_data, main="Autocorrelation (ACF)", col="black", lwd=2)
pacf(sim_data, main="Partial Autocorrelation (PACF)", col="brown", lwd=2)

# Close PNG saving
dev.off()
