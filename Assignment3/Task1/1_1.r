# Load libraries
library(arima2)

sapply(dir("Assignment3/functions", full.names=TRUE), source)

#Simulate the AR(2) process 5 times with 200 observations each
set.seed(123) # For reproducibility
phi <- c(0.6, -0.5) #Flipped because of sim function
m <- list()
m$ar <- phi
n <- 200
sims <- list()
for (i in 1:5){
  sims[[i]] <- sim(m, n)
}

#Generate and save plots in a 3x2 grid
par(mfrow=c(3,2))
for (i in 1:5){
  plot(sims[[i]], type="l", main=paste("Simulation", i), ylab="Value", xlab="Time")
}
# Save the plots to a file
png("Assignment3/Task1/plots/simulations.png", width = 1200, height = 800, res = 150)
dev.off()

#Generate ACF plots for each simulation
acf_data <- list()
ideal_acf <- generate_acf(m)
for (i in 1:5) {
    acf_data[[i]] <- acf(sims[[i]], plot=FALSE)$acf
}

# Save the ACF plots to a file
png(
    filename = "Assignment3/Task1/plots/acf_comparison.png",
    width = 1600,
    height = 3600, # Increased height to make each subplot 3 times as tall
    res = 200
)

# Plot ACF and ideal ACF against each other for each simulation individually
par(mfrow = c(5, 1)) # Adjusted layout to have one plot per row
for (i in 1:5) {
    plot(
        acf_data[[i]],
        type = "h",
        col = "blue",
        lwd = 2,
        ylim = range(c(ideal_acf, acf_data[[i]])),
        main = paste("ACF Comparison - Simulation", i),
        xlab = "Lag",
        ylab = "ACF"
    )
    points(
        ideal_acf,
        type = "h",
        col = rgb(1, 0, 0, 0.5), # Red with transparency
        lwd = 2
    )
    legend(
        "topright",
        legend = c("Simulated ACF", "Ideal ACF"),
        col = c("blue", rgb(1, 0, 0, 0.5)),
        lty = 1,
        lwd = 2
    )
}
dev.off()