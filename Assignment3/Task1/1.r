sapply(dir("Assignment3/functions", full.names=TRUE), source)

#Task 1.1
#Simulate the AR(2) process 5 times with 200 observations each
set.seed(123) # For reproducibility

# Define the AR(2) model
phi <- c(0.6, -0.5) #Flipped because of sim function
m <- list()
m$ar <- phi
n <- 200
sims <- list()
for (i in 1:5){
  sims[[i]] <- sim(m, n) #Sim taken from functions/sim.r
}

# Taks 1.2
# Function to generate and save plots
generate_and_save_plots <- function(sims, filename) {
    png(
      filename = filename, 
        width = 1600,
        height = 2700, 
        res = 200
    )

  par(mfrow = c(3, 2))
  for (i in seq_along(sims)) {
    plot(
      sims[[i]], 
      type = "l", 
      main = paste("Simulation", i), 
      ylab = "Value", 
      xlab = "Time"
    )
  }

  dev.off()
}

# Call the function
generate_and_save_plots(sims, "Assignment3/Task1/plots/simulations1_1.png")

#Generate ACF plots for each simulation
acf_data <- list()
ideal_acf <- generate_acf(m)
for (i in 1:5) {
    acf_data[[i]] <- acf(sims[[i]], plot=FALSE)$acf
}

# Function to plot ACF and ideal ACF and save to a file
plot_acf_comparison <- function(acf_data, ideal_acf, filename) {
  # Save the ACF plots to a file
  png(
    filename = filename,
    width = 1600,
    height = 3600, 
    res = 200
  )
  
  # Plot ACF and ideal ACF against each other for each simulation individually
  par(mfrow = c(5, 1)) # Adjusted layout to have one plot per row
  for (i in seq_along(acf_data)) {
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
}

plot_acf_comparison(acf_data, ideal_acf, "Assignment3/Task1/plots/acf_comparison1_2.png")

#Task 1.3
m$ar <- c(-0.6,0.3) #Flipped because of sim function
sims <- list()
for (i in 1:5){
  sims[[i]] <- sim(m, n) #Sim taken from functions/sim.r
}

acf_data <- list()
ideal_acf <- generate_acf(m)
for (i in 1:5) {
    acf_data[[i]] <- acf(sims[[i]], plot=FALSE)$acf
}

generate_and_save_plots(sims, "Assignment3/Task1/plots/simulations1_3.png")
plot_acf_comparison(acf_data, ideal_acf, "Assignment3/Task1/plots/acf_comparison1_3.png")

#Task 1.4
m$ar <- c(0.7,-0.3) #Flipped because of sim function
sims <- list()
for (i in 1:5){
  sims[[i]] <- sim(m, n) #Sim taken from functions/sim.r
}

acf_data <- list()
ideal_acf <- generate_acf(m)
for (i in 1:5) {
    acf_data[[i]] <- acf(sims[[i]], plot=FALSE)$acf
}

generate_and_save_plots(sims, "Assignment3/Task1/plots/simulations1_4.png")
plot_acf_comparison(acf_data, ideal_acf, "Assignment3/Task1/plots/acf_comparison1_4.png")

#Task 1.4
m$ar <- c(0.75,-0.3) #Flipped because of sim function
sims <- list()
for (i in 1:5){
  sims[[i]] <- sim(m, n) #Sim taken from functions/sim.r
}

acf_data <- list()
ideal_acf <- generate_acf(m)
for (i in 1:5) {
    acf_data[[i]] <- acf(sims[[i]], plot=FALSE)$acf
}

generate_and_save_plots(sims, "Assignment3/Task1/plots/simulations1_5.png")
plot_acf_comparison(acf_data, ideal_acf, "Assignment3/Task1/plots/acf_comparison1_5.png")