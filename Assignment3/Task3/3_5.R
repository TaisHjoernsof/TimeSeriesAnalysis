library(ggplot2)
library(dplyr)
library(patchwork)

# Load data
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Train/test split
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

# Fit linear model
lm_model <- lm(Ph ~ Tdelta + Gv, data = train_data)
summary(lm_model)

# Add predictions and residuals
train_data$Ph_hat <- predict(lm_model)
train_data$residuals <- train_data$Ph - train_data$Ph_hat

# Define clean theme
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.175, 0.905),      # Top-left corner inside the plot
    legend.direction = "horizontal",
    legend.key = element_blank(),         # No box around legend keys
    legend.background = element_blank()   # Remove background box
  )

# Plot: Actual vs. Predicted
p1 <- ggplot(train_data, aes(x = Time)) +
  geom_line(aes(y = Ph, color = "Actual")) +
  geom_line(aes(y = Ph_hat, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "black")) +
  labs(title = "Actual vs. Predicted Heater Power", y = "Ph", x = "") +
  tight_theme

# Plot: Residuals over time
p2 <- ggplot(train_data, aes(x = Time, y = residuals)) +
  geom_line(color = "gray20") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals of Linear Model", y = "Residual", x = "Time") +
  tight_theme

# Display both side-by-side
p1 / p2

# Combine ACF and CCF plots side-by-side in base R
par(
  mfrow = c(1, 3),        # 3 plots side by side
  mar = c(5, 5, 4, 2) + 0.1,  # bottom, left, top, right
  oma = c(0, 0, 2, 0),    # outer margin
  cex.main = 2,           # title size
  cex.lab = 1.6,          # axis label size
  cex.axis = 1.4          # tick label size
)

# ACF of residuals
acf(train_data$residuals, main = "ACF: Residuals")

# CCF: residuals vs. Tdelta
ccf(train_data$residuals, train_data$Tdelta, main = "CCF: Residuals vs. Tdelta")

# CCF: residuals vs. Gv
ccf(train_data$residuals, train_data$Gv, main = "CCF: Residuals vs. Gv")

# Reset plotting layout
par(mfrow = c(1, 1))
