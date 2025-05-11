source("Assignment4/Task1/1_1.r")
library(ggplot2)
library(reshape2)

plotFolder <- "Assignment4/plots/Task1"

# Reshape the data for ggplot
X_df <- as.data.frame(t(X))
nms <- paste0("X[", seq_len(ncol(X_df)), "]")
X_df$Time <- 1:n
X_long <- melt(X_df, id.vars = "Time", variable.name = "Series", value.name = "Value")

# Plot using ggplot
ggplot(X_long, aes(x = Time, y = Value, color = Series)) +
  geom_line() +
  labs(x = "Time", y = "X", title = "Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the plot
ggsave(paste(plotFolder, "/1_1.png",sep = ""), width = 8, height = 6)