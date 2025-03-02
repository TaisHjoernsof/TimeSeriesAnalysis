# Load required packages
library(ggplot2)
library(readr)
library(dplyr)

# Read the dataset
data <- read_csv("/Users/jonaswiendl/Desktop/TSA_Code/Assignment1/DST_BIL54.csv")

# Convert 'time' column from character to Date
data$time <- as.Date(paste0(data$time, "-01"), format="%Y-%m-%d")

# Create the time variable x (normalized time since 2018)
data <- data %>%
  mutate(x = as.numeric(format(time, "%Y")) + (as.numeric(format(time, "%m")) - 1) / 12)

# Split into training (2018-Jan to 2023-Dec) and test (2024-Jan to 2024-Dec) sets
train_data <- data %>% filter(time < as.Date("2024-01-01"))
test_data <- data %>% filter(time >= as.Date("2024-01-01"))

# Add a new column to distinguish between training and test sets
data <- data %>%
  mutate(set = ifelse(time < as.Date("2024-01-01"), "Training", "Test"))

# Ensure 'set' is treated as a factor for proper color mapping
data$set <- as.factor(data$set)

# Find the last training point and first test point
last_train <- train_data %>% filter(time == max(time)) # Last training point
first_test <- test_data %>% filter(time == min(time))  # First test point

ggplot(data, aes(x = x, y = total / 1e6, color = set)) +  # Convert to millions
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("Training" = "blue", "Test" = "red")) +
  
  # Connect last training point to first test point with a solid grey line
  geom_segment(aes(x = last_train$x, y = last_train$total / 1e6, 
                   xend = first_test$x, yend = first_test$total / 1e6),
               color = "grey") +
  
  # Format x-axis to show yearly ticks
  scale_x_continuous(breaks = seq(floor(min(data$x)), ceiling(max(data$x)), by = 1)) +
  
  # Format y-axis to show values in decimal (millions)
  scale_y_continuous(labels = scales::label_number(scale = 1, accuracy = 0.1)) +
  
  # Labels
  labs(title = "Time Series of Total Vehicles in Denmark",
       x = "Time (years)", y = "Total Vehicles (millions)",
       color = "") +
  
  # Theme modifications
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),  # Center title, increase size
    axis.title = element_text(size = 24),  # Increase axis titles size
    axis.text = element_text(size = 20),    # Increase axis values size
    legend.position = c(0.86, 0.3),  # Move legend inside plot (x=left-right, y=bottom-top)
    legend.text = element_text(size = 20),  # Increase legend text size
  )

