# Load data
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Define training and test sets
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

# Fit ARX(2) model: include lagged outputs and inputs
arx2_model <- lm(Ph ~ Ph.l1 + Ph.l2 + Tdelta + Tdelta.l1 + Gv + Gv.l1, data = train_data)

# Predict on training set
train_data$Ph_hat_arx2 <- predict(arx2_model)

# Residuals
train_data$resid_arx2 <- train_data$Ph - train_data$Ph_hat_arx2

# Define custom ggplot theme
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.175, 0.90),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Plot: Actual vs. Predicted Ph
p1 <- ggplot(train_data, aes(x = Time)) +
  geom_line(aes(y = Ph, color = "Actual")) +
  geom_line(aes(y = Ph_hat_arx2, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "black")) +
  labs(title = "ARX(2) Model: Actual vs. Predicted Ph", y = "Heater Power (W)", x = "Time") +
  tight_theme

# Plot: Residuals
p2 <- ggplot(train_data, aes(x = Time, y = resid_arx2)) +
  geom_line(color = "gray20") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "ARX(2) Model: Residuals", y = "Residual", x = "Time") +
  tight_theme

p1 / p2

# Load required libraries
library(ggplot2)
library(dplyr)

# Set max ARX order to consider
max_order <- 10

# Prepare vectors to store AIC and BIC
aic_vals <- numeric(max_order)
bic_vals <- numeric(max_order)

# Loop through model orders 1 to max_order
for (k in 1:max_order) {
  # Construct formula dynamically
  formula_str <- paste0(
    "Ph ~ ",
    paste0("Ph.l", 1:k, collapse = " + "), " + ",
    paste0("Tdelta", c("", paste0(".l", 1:(k - 1))), collapse = " + "), " + ",
    paste0("Gv",     c("", paste0(".l", 1:(k - 1))),     collapse = " + ")
  )
  
  print(formula_str)
  
  # Fit model and extract AIC/BIC
  model_k <- lm(as.formula(formula_str), data = train_data)
  aic_vals[k] <- AIC(model_k)
  bic_vals[k] <- BIC(model_k)
}

# Create dataframe for plotting
criteria_df <- data.frame(
  Order = 1:max_order,
  AIC = aic_vals,
  BIC = bic_vals
)

tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.175, 0.90),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Plot: AIC and BIC vs. Model Order
ggplot(criteria_df, aes(x = Order)) +
  geom_line(aes(y = AIC, color = "AIC"), size = 1.2) +
  geom_point(aes(y = AIC, color = "AIC"), size = 2.5) +
  geom_line(aes(y = BIC, color = "BIC"), size = 1.2) +
  geom_point(aes(y = BIC, color = "BIC"), size = 2.5) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_color_manual(values = c("AIC" = "blue", "BIC" = "darkred")) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "AIC and BIC vs. ARX Model Order",
    x = "Model Order",
    y = "Criterion Value"
  ) +
  tight_theme

