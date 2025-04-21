library(ggplot2)
library(dplyr)

# Load your data
data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Split into training and test set
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

# Maximum ARX model order to evaluate
max_order <- 10
rmse_vec <- numeric(max_order)

# Loop over model orders
for (k in 1:max_order) {
  # Skip if not enough test data
  if (nrow(test_data) < k) next
  
  # Build formula dynamically
  formula_str <- paste0(
    "Ph ~ ",
    paste0("Ph.l", 1:k, collapse = " + "), " + ",
    paste0("Tdelta", c("", paste0(".l", 1:(k - 1))), collapse = " + "), " + ",
    paste0("Gv",     c("", paste0(".l", 1:(k - 1))), collapse = " + ")
  )
  
  # Fit model on training data
  arx_model <- lm(as.formula(formula_str), data = train_data)
  
  # Predict on test data (one-step ahead)
  test_rows <- (k + 167):nrow(data)  # time steps where we have full lag info
  print(test_rows)
  predictions <- predict(arx_model, newdata = data[test_rows, ])
  
  # Compute RMSE on test period
  actual <- data$Ph[test_rows]
  rmse <- sqrt(mean((actual - predictions)^2))
  rmse_vec[k] <- rmse
}

# Prepare data frame for plotting
rmse_df <- data.frame(Order = 1:max_order, RMSE = rmse_vec)

# Custom theme consistent with earlier plots
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(2, 10, 2, 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",  # No legend needed
    legend.key = element_blank(),
    legend.background = element_blank()
  )

# Final RMSE vs. Order plot
ggplot(rmse_df, aes(x = Order, y = RMSE)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_vline(xintercept = which.min(rmse_df$RMSE), linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = 1:max_order) +
  labs(
    title = "Test RMSE vs. ARX Model Order",
    x = "Model Order",
    y = "Test RMSE"
  ) +
  tight_theme



