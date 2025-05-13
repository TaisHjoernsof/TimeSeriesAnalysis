source("Assignment4/Task1/1_2.R")
source("Assignment4/functions/plot_functions.R")
library(ggplot2)
library(reshape2)
library(latex2exp)

plotFolder <- "Assignment4/plots/Task1"

# Create a data frame for plotting
data <- data.frame(
    Index = 1:n,
    X = X[1,],
    Y = Y
)

#t <- TeX(r"(Latent State Trajectory $X$ and Observed State Trajectory $Y=X+e_{2,t},~~~e_{2,t} \sim N(0,\sigma_2^2=1)$)")

set_plot_style()
# Plot using ggplot
ggplot(data, aes(x = Index)) +
    geom_line(aes(y = X, color = "X_V1"), linetype = "dashed") +
    geom_line(aes(y = Y, color = "Y")) +
    labs(color = "Legend") +
    theme_bw() +
    scale_y_continuous(limits = c(0, 15), 
                    breaks = seq(0, 15, by = 5), 
                    minor_breaks = seq(0, 15, by = 1))

# Save the plot
ggsave(paste(plotFolder, "/1_2.png", sep = ""), width = 8, height = 6)