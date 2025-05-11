source("Assignment4/Task1/1_2.R")
library(ggplot2)
library(reshape2)

plotFolder <- "Assignment4/plots/Task1"

# Create a data frame for plotting
data <- data.frame(
    Index = 1:n,
    X1 = X[1,],
    Y = Y
)

# Plot using ggplot
ggplot(data, aes(x = Index)) +
    geom_line(aes(y = X1, color = "X"), linetype = "dashed") +
    geom_line(aes(y = Y, color = "Y")) +
    labs(color = "Legend") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save the plot
ggsave(paste(plotFolder, "/1_2.png", sep = ""), width = 8, height = 6)