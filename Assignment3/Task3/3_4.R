data <- read.csv("Documents/02417_TSA/TimeSeriesAnalysis/Assignment3/Task3/box_data_60min.csv")
data$Time <- 1:nrow(data)

# Define training and test sets (first 167 observations = training)
train_data <- data %>% filter(Time <= 167)
test_data  <- data %>% filter(Time > 167)

# Set maximum lag
max_lag <- 10

# Compute CCF between Tdelta and Ph (input → output)
ccf_td_ph <- ccf(train_data$Tdelta, train_data$Ph, lag.max = max_lag, plot = FALSE)

# Compute CCF between Gv and Ph (input → output)
ccf_gv_ph <- ccf(train_data$Gv, train_data$Ph, lag.max = max_lag, plot = FALSE)

# Convert to data frames for plotting
df_td <- data.frame(lag = ccf_td_ph$lag, irf = ccf_td_ph$acf)
df_gv <- data.frame(lag = ccf_gv_ph$lag, irf = ccf_gv_ph$acf)

# Filter only positive lags (including lag 0)
df_td_pos <- df_td %>% filter(lag >= 0)
df_gv_pos <- df_gv %>% filter(lag >= 0)

# Plot IRF: Tdelta → Ph (positive lags only)
plot_td <- ggplot(df_td_pos, aes(x = lag, y = irf)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  scale_x_continuous(breaks = seq(0, max_lag, by = 1)) +
  theme_minimal() +
  labs(title = "Impulse Response: Tdelta → Ph", x = "Lag", y = "CCF/IRF") +
  geom_hline(yintercept = 0, color = "black")

# Plot IRF: Gv → Ph (positive lags only)
plot_gv <- ggplot(df_gv_pos, aes(x = lag, y = irf)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.6) +
  scale_x_continuous(breaks = seq(0, max_lag, by = 1)) +
  theme_minimal() +
  labs(title = "Impulse Response: Gv → Ph", x = "Lag", y = "CCF/IRF") +
  geom_hline(yintercept = 0, color = "black")

# Combine side by side
library(patchwork)
plot_td + plot_gv



library(vars)

# Create multivariate time series
ts_data <- ts(train_data[, c("Ph", "Tdelta", "Gv")])

# Fit VAR model
var_model <- VAR(ts_data, p = 5, type = "const")

# Estimate impulse response from Tdelta to Ph
irf_tdelta <- irf(var_model, impulse = "Tdelta", response = "Ph", n.ahead = 10)

# Plot
plot(irf_tdelta)

library(ggplot2)
library(patchwork)

# Define custom theme
tight_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none"
  )

# Plot IRF: Tdelta → Ph
plot_td <- ggplot(df_td_pos, aes(x = lag, y = irf)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(breaks = seq(0, max_lag, by = 1)) +
  labs(title = "Impulse Response: Tdelta → Ph", x = "Lag", y = "CCF/IRF") +
  tight_theme

# Plot IRF: Gv → Ph
plot_gv <- ggplot(df_gv_pos, aes(x = lag, y = irf)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.6) +
  geom_hline(yintercept = 0, color = "black") +
  scale_x_continuous(breaks = seq(0, max_lag, by = 1)) +
  labs(title = "Impulse Response: Gv → Ph", x = "Lag", y = "CCF/IRF") +
  tight_theme

# Combine side by side
plot_td + plot_gv


