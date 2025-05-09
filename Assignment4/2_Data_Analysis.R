# Load required packages
library(tidyverse)
library(lubridate)


# setwd("Documents/02417_TSA/TimeSeriesAnalysis/Assignment4")

# Load functions
source("functions/plot_functions.R")

# Apply global plot style
set_plot_style()

# Load data
df <- read_csv("data/transformer_data.csv")

# Check structure
glimpse(df)

# Rename for clarity
df <- df %>%
  rename(
    time = t,
    temperature = Yt,
    outdoor_temp = Ta_t,
    solar_rad = Phi_s_t,
    load = Phi_I_t
  )

# Option 1: Plot each variable in its own panel (long format)
df_long <- df %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")


variable_labels <- c(
  "Y"  = "Transformer Temp (°C)",
  "Ta" = "Outdoor Temp (°C)",
  "S"  = "Solar Radiation (W/m²)",
  "I"  = "Transformer Load (kA)"
)

ggplot(df_long, aes(x = time, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~ variable, 
             scales = "free_y", 
             ncol = 1,
             labeller = as_labeller(variable_labels)) +
  labs(
    title = "Transformer Station Data Overview",
    x = "Time (hours)",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )

unique(df_long$variable)


