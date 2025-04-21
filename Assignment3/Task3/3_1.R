library(ggplot2)
library(dplyr)
library(patchwork)

# Load the data
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Mark training vs test set
data$Set <- ifelse(data$Time <= 167, "Train", "Test")

# Custom theme with minimal spacing and legend styling
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.1, 0.905),      # Top-left corner inside the plot
    legend.direction = "horizontal",
    legend.key = element_blank(),         # No box around legend keys
    legend.background = element_blank()   # Remove background box
  )

# Plot 1: Ph
p1 <- ggplot(data, aes(x = Time, y = Ph, linetype = Set)) +
  geom_line(color = "red") +
  scale_linetype_manual(values = c("Train" = "solid", "Test" = "dashed")) +
  labs(title = "Heater Power (Ph)", y = "W", x = "") +
  tight_theme

# Plot 2: Tdelta
p2 <- ggplot(data, aes(x = Time, y = Tdelta, linetype = Set)) +
  geom_line(color = "blue") +
  scale_linetype_manual(values = c("Train" = "solid", "Test" = "dashed")) +
  labs(title = "Temperature Difference (Tdelta)", y = "°C", x = "") +
  tight_theme

# Plot 3: Gv
p3 <- ggplot(data, aes(x = Time, y = Gv, linetype = Set)) +
  geom_line(color = "darkgreen") +
  scale_linetype_manual(values = c("Train" = "solid", "Test" = "dashed")) +
  labs(title = "Solar Radiation (Gv)", y = "W/m²", x = "Time (hours)") +
  tight_theme

# Combine all plots vertically
(p1 / p2 / p3) + 
  plot_layout(ncol = 1, heights = c(1, 1, 1)) &
  theme(plot.margin = margin(2, 10, 2, 10))
