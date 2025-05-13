source("Assignment4/Task1/1_1.r")
source("Assignment4/functions/plot_functions.R")
library(ggplot2)
library(reshape2)
library(latex2exp)


plotFolder <- "Assignment4/plots/Task1"

# Reshape the data for ggplot
X_df <- as.data.frame(t(X))
nms <- paste0("X[", seq_len(ncol(X_df)), "]")
X_df$Time <- 1:n
X_long <- melt(X_df, id.vars = "Time", variable.name = "Series", value.name = "Value")


set_plot_style()
#title <- TeX(r"(5 Realizations of $x_{t}=0.9x_{t-1} + 1 + e_{1,t},~~~e_{1,t} \sim N(0,\sigma^2=1)$)")
# Plot using ggplot
ggplot(X_long, aes(x = Time, y = Value, color = Series)) +
  geom_line() +
  labs(x = "Time", y = "X") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, by = 5), 
                     minor_breaks = seq(0, 15, by = 1))
# Save the plot
ggsave(paste(plotFolder, "/1_1.png",sep = ""), width = 8, height = 6)