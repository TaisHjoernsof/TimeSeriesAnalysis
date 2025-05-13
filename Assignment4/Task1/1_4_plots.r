source("Assignment4/Task1/1_4.R")
library(latex2exp)
theta_1 <- TeX("$\\theta=(1,0.9,1)$")
theta_2 <- TeX("$\\theta=(5,0.9,1)$")
theta_3 <- TeX("$\\theta=(1,0.9,5)$")
# Boxplot for theta_opt_1, theta_opt_2, and theta_opt_3 in a single plot
sigma <- TeX("$\\sigma_1$")
png(
  filename = "Assignment4/plots/Task1/1_4_combined_boxplots.png", width = 1200, height = 480, res = 150
)

# Arrange plots in a 1x3 grid
par(mfrow = c(1, 3))

# Boxplot for theta_opt_1
a <- theta_opt_1[, 1]
b <- theta_opt_1[, 2]
c <- theta_opt_1[, 3]
boxplot(
  a, b, c,
  names = c("a", "b", sigma),
  ylab = "Parameter Values",
  col = c("lightblue", "lightgreen", "pink"),
  ylim = c(0, 6),
  main = theta_1
)

# Boxplot for theta_opt_2
a2 <- theta_opt_2[, 1]
b2 <- theta_opt_2[, 2]
c2 <- theta_opt_2[, 3]
boxplot(
  a2, b2, c2,
  names = c("a", "b", sigma),
  col = c("lightblue", "lightgreen", "pink"),
  ylim = c(0, 6),
  main = theta_2
)

# Boxplot for theta_opt_3
a3 <- theta_opt_3[, 1]
b3 <- theta_opt_3[, 2]
c3 <- theta_opt_3[, 3]
boxplot(
  a3, b3, sqrt(c3),
  names = c("a", "b", sigma),
  col = c("lightblue", "lightgreen", "pink"),
  ylim = c(0, 6),
  main = theta_3
)

dev.off()