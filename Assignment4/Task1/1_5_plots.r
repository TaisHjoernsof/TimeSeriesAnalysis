source("Assignment4/Task1/1_5.r")

# Define the degrees of freedom
df_values <- c(100, 5, 2, 1)
t_100 <- TeX("$\\nu = 100$")
t_5 <- TeX("$\\nu = 5$")
t_2 <- TeX("$\\nu = 2$")
t_1 <- TeX("$\\nu = 1$")
sigma <- TeX("$\\sigma_1$")

T <- c(t_100, t_5, t_2, t_1)
# Create plots for normal and student_t distributions
# Set up a 1x4 plotting layout for density plots
png("Assignment4/plots/Task1/1_5_density_plots_combined.png", width = 1200, height = 400, res = 150)
par(mfrow = c(1, 4))  # Set up a 1x4 plotting layout

for (i in seq_along(df_values)) {
    df <- df_values[i]
    
    # Generate x values
    x <- seq(-10, 10, length.out = 1000)
    
    # Calculate densities
    normal_density <- dnorm(x)
    student_t_density <- dt(x, df = df)
    
    # Plot densities
    plot(x, normal_density, type = "l", col = "blue", lwd = 2, 
         main = T[i], xlab = "x", ylab = "Density")
    lines(x, student_t_density, col = "red", lwd = 2)
    legend("topright", legend = c("Normal", "Student-t"), 
           col = c("blue", "red"), lwd = 2, cex = 0.8)
}

# Reset plotting layout and save the plot
par(mfrow = c(1, 1))
dev.off()

# Combine all boxplots into a single horizontally stacked plot
png("Assignment4/plots/Task1/1_5_boxplots_combined.png", width = 1200, height = 400, res = 150)
par(mfrow = c(1, 4))  # Set up a 1x4 plotting layout


# Plot each boxplot
boxplot(theta_opt_1, main = t_100, names = c("a","b",sigma),ylim=c(0,6), col = "lightblue")
boxplot(theta_opt_2, main = t_5, names = c("a","b",sigma),ylim=c(0,6), col = "lightgreen")
boxplot(theta_opt_3, main = t_2, names = c("a","b",sigma),col = "lightpink")
boxplot(theta_opt_4, main = t_1, names = c("a","b",sigma),col = "lightyellow")

# Reset plotting layout and save the plot
par(mfrow = c(1, 1))
dev.off()
