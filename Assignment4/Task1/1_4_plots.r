source("Assignment4/Task1/1_4.R")

# Boxplot for theta_opt_1
png("Assignment4/plots/Task1/1_4_boxplot1.png")
a <- theta_opt_1[, 1]
b <- theta_opt_1[, 2]
c <- theta_opt_1[, 3]
boxplot(
  a, b, c,
  names = c("a", "b", "c"), 
  main = "Boxplot of Optimized Parameters (theta_opt_1)",
  ylab = "Parameter Values", 
  col = c("lightblue", "lightgreen", "pink")
)
dev.off()

# Boxplot for theta_opt_2
png("Assignment4/plots/Task1/1_4_boxplot2.png")
a2 <- theta_opt_2[, 1]
b2 <- theta_opt_2[, 2]
c2 <- theta_opt_2[, 3]
boxplot(
  a2, b2, c2,
  names = c("a", "b", "c"), 
  main = "Boxplot of Optimized Parameters (theta_opt_2)",
  ylab = "Parameter Values", 
  col = c("lightblue", "lightgreen", "pink")
)
dev.off()

# Boxplot for theta_opt_3
png("Assignment4/plots/Task1/1_4_boxplot3.png")
a3 <- theta_opt_3[, 1]
b3 <- theta_opt_3[, 2]
c3 <- theta_opt_3[, 3]
boxplot(
  a3, b3, sqrt(c3),
  names = c("a", "b", "c"), 
  main = "Boxplot of Optimized Parameters (theta_opt_3)",
  ylab = "Parameter Values", 
  col = c("lightblue", "lightgreen", "pink")
)
dev.off()