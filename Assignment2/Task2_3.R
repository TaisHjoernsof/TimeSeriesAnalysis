# Load necessary library
library(forecast)

# Clear previous plots and reset layout
graphics.off()  # Close all previous plots
par(mfrow=c(1,1))  # Reset layout

# Set model parameters
p <- 1  # AR order (non-seasonal)
d <- 0  # Integration order (differencing)
q <- 0  # MA order

# Seasonal part
P <- 0  # Seasonal AR order
D <- 0  # Seasonal differencing order
Q <- 1  # Seasonal MA order
Period <- 12  # Seasonality

# Define fixed AR and SMA coefficients
phi <- 0.9  # Non-seasonal AR(1) coefficient
Theta <- -0.7  # Seasonal MA(1) coefficient at lag 12

# Simulate ARIMA process
n <- 200  # Number of observations
sim_data <- arima.sim(model = list(order = c(p,d,q), 
                                   seasonal = list(order = c(P,D,Q), period = Period), 
                                   ar = phi, 
                                   seasonal.ma = Theta), 
                      n = n)

# Define layout: Time series on top, ACF & PACF below side by side
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))

# Plot Time Series
plot.ts(sim_data, main="AR(1) with Seasonal MA(1): ARIMA(1,0,0) × (0,0,1)[12], ϕ_1 = 0.9, Θ_1 = -0.7", 
        ylab="Value", xlab="Time", col="black", lwd=1.2)

# Plot ACF (left)
acf(sim_data, main="ACF: ARIMA(1,0,0) × (0,0,1)[12] with ϕ₁ = 0.9, Θ₁ = -0.7", col="black", lwd=2)

# Plot PACF (right)
pacf(sim_data, main="PACF: ARIMA(1,0,0) × (0,0,1)[12] with ϕ₁ = 0.9, Θ₁ = -0.7", col="brown", lwd=2)

# Reset layout
par(mfrow=c(1,1))
