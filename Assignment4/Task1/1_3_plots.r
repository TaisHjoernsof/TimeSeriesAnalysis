library(ggplot2)
source("Assignment4/Task1/1_3.r")

plotFolder <- "Assignment4/plots/Task1"

z_95 <- qnorm(0.975)

data <- data.frame(
    Time = 1:n,
    Observed = Y,
    Predicted = kalman$x_pred,
    TrueState = X[1, ],
    Lower_interval = x_pred_lower,
    Upper_interval = x_pred_upper
)

ggplot(data, aes(x = Time)) +
    geom_line(aes(y = Observed, color = "Observed (Y)"), linetype="dashed") +
    geom_line(aes(y = Predicted, color = "Kalman Predicted (X_hat)")) +
    geom_line(aes(y = TrueState, color = "True State (X)")) +
    geom_ribbon(aes(ymin = Lower_interval, ymax = Upper_interval), alpha = 0.4, fill = "#7ec0d6") +
    labs(
        title = "Kalman Filter Results",
        x = "Time",
        y = "Values",
        color = "Legend"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the plot
ggsave(paste(plotFolder, "/1_3.png", sep = ""), width = 8, height = 6)