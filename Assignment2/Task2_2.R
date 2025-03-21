graphics.off()  # Close any existing plots
par(mfrow=c(1,1))  # Reset layout settings
#dev.new()  # Open a new plotting window (optional)

# Load necessary library
library(forecast)

# Set model parameters
p <- 0  # AR order (non-seasonal)
d <- 0  # Integration order (differencing)
q <- 0  # MA order

# Seasonal part
P <- 1  # Seasonal AR order
D <- 0  # Seasonal differencing order
Q <- 0  # Seasonal MA order
Period <- 12  # Seasonality

# Define fixed seasonal AR coefficient
Phi <- -0.9  # Seasonal AR(1) coefficient at lag 12

# Simulate seasonal ARIMA process
n <- 200  # Number of observations
sim_data <- arima.sim(model = list(order = c(p,d,q), 
                                   seasonal = list(order = c(P,D,Q), period = Period), 
                                   seasonal.ar = Phi), 
                      n = n)

# Define layout: Time series on top, ACF & PACF below side by side
layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE), heights = c(1.5, 1.5))

# Plot Time Series
plot.ts(sim_data, main="Simulated ARIMA(0,0,0) × (1,0,0)[12] with Φ_1 = -0.9", 
        ylab="Value", xlab="Time", col="black", lwd=1.2)

# Plot ACF (left)
acf(sim_data, main="Autocorrelation (ACF)", col="black", lwd=2)

# Plot PACF (right)
pacf(sim_data, main="Partial Autocorrelation (PACF)", col="brown", lwd=2)

# Reset layout
par(mfrow=c(1,1))

