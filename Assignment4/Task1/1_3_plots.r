library(ggplot2)
source("Assignment4/Task1/1_3.r")
source("Assignment4/functions/plot_functions.R")

plot_folder <- "Assignment4/plots/Task1"

#set_plot_style()

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
    theme_bw() +
    geom_line(
        aes(y = TrueState, color = "True State (X)")
      ) +
      geom_line(
        aes(y = Observed, color = "Observed (Y)"),
        linetype = "dashed"
      ) +
      geom_line(
        aes(y = Predicted, color = "Kalman Predicted (X_hat)")
      ) +
      geom_ribbon(
        aes(
          ymin = Lower_interval,
          ymax = Upper_interval
        ),
        fill = "#0798078c", # Match the color of Kalman Predicted (X_hat)
        alpha = 0.4
      ) +
      labs(
        x = "Time",
        y = "Values",
        color = "Legend",
        fill = "Legend"
      ) +
      scale_color_manual(
        values = c(
          "True State (X)" = "#f8766d",
          "Observed (Y)" = "#01bfc4",
          "Kalman Predicted (X_hat)" = "#0798078c"
        ),
        breaks = c(
          "True State (X)",
          "Observed (Y)",
          "Kalman Predicted (X_hat)"
        )
      ) +
      scale_y_continuous(
        limits = c(-1.2, 15.15),
        breaks = seq(0, 15, by = 5),
        minor_breaks = seq(0, 15, by = 1),
        expand = c(0, 0)
      )
      

# Save the plot
ggsave(paste(plot_folder, "/1_3.png", sep = ""), width = 8, height = 6)