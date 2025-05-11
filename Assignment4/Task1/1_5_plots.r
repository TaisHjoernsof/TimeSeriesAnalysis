source("Assignment4/Task1/1_5.r")

# Define the degrees of freedom
df_values <- c(100, 5, 2, 1)

# Create plots for normal and student_t distributions
for (i in seq_along(df_values)) {
    df <- df_values[i]
    
    # Generate x values
    x <- seq(-10, 10, length.out = 1000)
    
    # Calculate densities
    normal_density <- dnorm(x)
    student_t_density <- dt(x, df = df)
    
    # Save the plot
    png(paste0("Assignment4/plots/Task1/1_5_distribution_plot_", df, ".png"))
    plot(x, normal_density, type = "l", col = "blue", lwd = 2, 
             main = paste("Normal vs Student-t (df =", df, ")"), 
             xlab = "x", ylab = "Density")
    lines(x, student_t_density, col = "red", lwd = 2)
    legend("topright", legend = c("Normal", "Student-t"), 
                 col = c("blue", "red"), lwd = 2)
    dev.off()
}

png("Assignment4/plots/Task1/1_5_boxplot_100.png")
boxplot(theta_opt_1, main = "Theta Opt 1", col = "lightblue")
dev.off()

png("Assignment4/plots/Task1/1_5_boxplot_5.png")
boxplot(theta_opt_2, main = "Theta Opt 2", col = "lightgreen")
dev.off()

png("Assignment4/plots/Task1/1_5_boxplot_2.png")
boxplot(theta_opt_3, main = "Theta Opt 3", col = "lightpink")
dev.off()

png("Assignment4/plots/Task1/1_5_boxplot_1.png")
boxplot(theta_opt_4, main = "Theta Opt 4", col = "lightyellow")
dev.off()
