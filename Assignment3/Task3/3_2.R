library(dplyr)
library(tidyverse)


# Define training and test sets (first 167 observations = training)
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

cor(train_data$Ph, train_data$Tdelta)  # expected to be positive
cor(train_data$Ph, train_data$Gv)      # expected to be negative
cor(train_data$Gv, train_data$Tdelta)      # expected to be negative

# ACF plots of each variable (training set)
par(mfrow = c(1, 3))  # 3 plots side by side
acf(train_data$Ph, main = "ACF: Heater Power (Ph)")
acf(train_data$Tdelta, main = "ACF: Temperature Diff (Tdelta)")
acf(train_data$Gv, main = "ACF: Solar Radiation (Gv)")
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))  # 3 plots side by side
ccf(train_data$Ph, train_data$Tdelta, main = "CCF: Ph vs. Tdelta")
ccf(train_data$Ph, train_data$Gv, main = "CCF: Ph vs. Gv")
ccf(train_data$Tdelta, train_data$Gv, main = "CCF: Tdelta vs. Gv")
par(mfrow = c(1, 1))

# Set layout: 2 rows, 3 columns
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))  # adjust margins if needed

# --- ACF plots ---
# Set up larger margins and font sizes
par(
  mfrow = c(1, 3),        # 3 plots side by side
  mar = c(5, 5, 4, 2) + 0.1,  # bottom, left, top, right
  oma = c(0, 0, 2, 0),    # outer margin
  cex.main = 2,           # title size
  cex.lab = 1.6,          # axis label size
  cex.axis = 1.4          # tick label size
)

# Plot ACFs
acf(train_data$Ph, main = "ACF: Heater Power (Ph)")
acf(train_data$Tdelta, main = "ACF: Temperature Diff (Tdelta)")
acf(train_data$Gv, main = "ACF: Solar Radiation (Gv)")

# Reset layout
par(mfrow = c(1, 1))



# --- CCF plots ---
ccf(train_data$Ph, train_data$Tdelta, main = "CCF: Ph vs. Tdelta")
ccf(train_data$Ph, train_data$Gv, main = "CCF: Ph vs. Gv")
ccf(train_data$Tdelta, train_data$Gv, main = "CCF: Tdelta vs. Gv")

# Reset layout to default
par(mfrow = c(1, 1))


pairs(train_data[, c("Ph", "Tdelta", "Gv")],
      main = "Scatter Plot Matrix (Training Set)",
      pch = 20, col = rgb(0.2, 0.2, 0.8, 0.5))

library(ggplot2)
library(patchwork)

# Scatter Plot 1: Ph vs. Tdelta
p1 <- ggplot(train_data, aes(x = Tdelta, y = Ph)) +
  geom_point(color = "red", alpha = 0.6, size = 1.8) +
  labs(title = "Ph vs. Tdelta", x = "Tdelta (°C)", y = "Ph (W)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Scatter Plot 2: Ph vs. Gv
p2 <- ggplot(train_data, aes(x = Gv, y = Ph)) +
  geom_point(color = "darkgreen", alpha = 0.6, size = 1.8) +
  labs(title = "Ph vs. Gv", x = "Gv (W/m²)", y = "Ph (W)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Scatter Plot 3: Tdelta vs. Gv
p3 <- ggplot(train_data, aes(x = Tdelta, y = Gv)) +
  geom_point(color = "blue", alpha = 0.6, size = 1.8) +
  labs(title = "Gv vs. Tdelta ", x = "Tdelta (°C)", y = "Gv (W/m²)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine the plots in one row
p1 + p2 + p3 + plot_layout(ncol = 3)


library(ggplot2)

ggplot(train_data, aes(x = Time)) +
  geom_line(aes(y = Tdelta, color = "Tdelta")) +
  geom_line(aes(y = Ph / 20, color = "Ph (scaled)")) +  # scaled for visibility
  scale_color_manual(values = c("Tdelta" = "blue", "Ph (scaled)" = "red")) +
  labs(title = "Overlay: Tdelta vs. Ph (scaled)", y = "", x = "Time") +
  theme_minimal()

library(patchwork)

# Define variables of interest
vars <- c("Ph", "Tdelta", "Gv")

# Create an empty 3x3 list matrix to hold plots
plot_matrix <- matrix(vector("list", 9), nrow = 3, ncol = 3)

# Fill diagonal with ACF plots
for (i in 1:3) {
  var <- vars[i]
  acf_data <- acf(train_data[[var]], plot = FALSE)
  df <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
  
  plot_matrix[[i, i]] <- ggplot(df, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", width = 0.1, fill = "gray30") +
    theme_minimal() +
    labs(title = paste("ACF of", var), x = NULL, y = NULL) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# Fill lower triangle with scatter plots
for (i in 2:3) {
  for (j in 1:(i-1)) {
    plot_matrix[[i, j]] <- ggplot(train_data, aes_string(x = vars[j], y = vars[i])) +
      geom_point(alpha = 0.5, color = "steelblue", size = 1) +
      theme_minimal() +
      theme(axis.title = element_text(size = 10))
  }
}

# Fill upper triangle with empty plots (or use plot_spacer)
for (i in 1:2) {
  for (j in (i+1):3) {
    plot_matrix[[i, j]] <- patchwork::plot_spacer()
  }
}

# Replace any NULLs (just in case) with blank
for (i in 1:3) {
  for (j in 1:3) {
    if (is.null(plot_matrix[[i, j]])) {
      plot_matrix[[i, j]] <- patchwork::plot_spacer()
    }
  }
}

# Combine into one plot using patchwork
final_plot <- wrap_plots(plot_matrix, nrow = 3, ncol = 3)

# Display the final plot
final_plot
