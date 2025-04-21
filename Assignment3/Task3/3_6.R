# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Load data
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Define training and test sets
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

# Fit ARX(1) model using lagged output (Ph.l0)
arx_model <- lm(Ph ~ Ph.l1 + Tdelta + Gv, data = train_data)
summary(arx_model)

# Predict and calculate residuals
train_data$Ph_hat_arx <- predict(arx_model)
train_data$resid_arx  <- train_data$Ph - train_data$Ph_hat_arx

# Define custom theme
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.1, 0.90),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Plot 1: Actual vs Predicted
p1 <- ggplot(train_data, aes(x = Time)) +
  geom_line(aes(y = Ph, color = "Actual")) +
  geom_line(aes(y = Ph_hat_arx, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "black")) +
  labs(title = "ARX(1) Model: Actual vs. Predicted Ph", y = "Heater Power (W)", x = "Time") +
  tight_theme

# Plot 2: Residuals
p2 <- ggplot(train_data, aes(x = Time, y = resid_arx)) +
  geom_line(color = "gray20") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals of ARX(1) Model", y = "Residual", x = "Time") +
  tight_theme

# Plot ACF and CCF in base R
par(mfrow = c(1, 3))
acf(train_data$resid_arx, main = "ACF: Residuals")
ccf(train_data$resid_arx, train_data$Tdelta, main = "CCF: Residuals vs. Tdelta")
ccf(train_data$resid_arx, train_data$Gv, main = "CCF: Residuals vs. Gv")
par(mfrow = c(1, 1))

# Combine ggplot outputs
p1 / p2
