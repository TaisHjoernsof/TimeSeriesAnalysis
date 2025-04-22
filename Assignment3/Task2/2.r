#Read datasolar.csv
solar <- read.csv("Assignment3/Task2/data/datasolar.csv")
#Import functions
sapply(dir("Assignment3/functions", full.names=TRUE), source)
#Create t-variable
Y = solar$power

#Create X-variable
my <- 5.72
X = log(Y) - my

phi <- -0.38
Phi <- -0.94
l <- length(X)
sigma2_e <- 0.22^2

e_hat <- (X + phi * lagvec(X,1)) * (X + Phi * lagvec(X,12))

#Remove all nan values from e_hat
e_hat <- e_hat[!is.na(e_hat)]

# Scatter plot of e_hat
png("Assignment3/Task2/plots/scatter.png")
plot(
  e_hat, 
  main = "Scatter Plot of e_hat", 
  xlab = "Index", 
  ylab = "e_hat", 
  pch = 16, 
  col = "blue"
)
#Line y = 0
abline(h = 0, col = "red", lty = 2, lwd = 2)
dev.off()

# QQ-plot of e_hat
png("Assignment3/Task2/plots/qqplot.png")
qqnorm(e_hat, main = "QQ-Plot of e_hat", pch = 16, col = "blue")
qqline(e_hat, col = "red", lty = 2, lwd = 2)
dev.off()

# Histogram of e_hat
png("Assignment3/Task2/plots/histogram.png")
hist(
  e_hat, 
  main = "Histogram of e_hat", 
  xlab = "e_hat", 
  ylab = "Frequency", 
  col = "blue", 
  border = "black"
)
# Add a normal curve
x <- seq(min(e_hat), max(e_hat), length = 100)
y <- dnorm(x, mean = mean(e_hat), sd = sd(e_hat))
lines(x, y * length(e_hat) * diff(hist(e_hat, plot = FALSE)$breaks)[1], col = "red", lwd = 2)
dev.off()

#Task 2.2
X_hat <- numeric(12)
for (k in 1:12){
    x_hat <- -phi * X[l - 1 + k] - Phi * X[l - 12 + k] - phi * Phi * X[l - 13 + k]
    X[l+k] <- x_hat
    X_hat[k] <- x_hat
}
Y_hat <- exp(X_hat + my)

# Plot Y with dashed forecasted values
png("Assignment3/Task2/plots/Y_plot.png")
plot(
    exp(X[1:36] + my), 
    type = "l", 
    main = "Plot of generated power with Forecasted Values", 
    xlab = "Index", 
    ylab = "Generation (MWh)", 
    col = "blue", 
    lwd = 2,
    xlim = c(0, 48)
)
lines(36:length(X), exp(X[36:length(X)]+my), col = "red", lty = 2, lwd = 2)
legend(
    "topright", 
    legend = c("Actual Values", "Forecasted Values"), 
    col = c("blue", "red"), 
    lty = c(1, 2), 
    lwd = 2
)
dev.off()


#Task 2.3
sigma2_k <- sigma2_e * (1 + phi^2)
X_lwr <- X_hat - 1.96 * sigma2_k
X_upr <- X_hat + 1.96 * sigma2_k
Y_lwr <- exp(X_lwr + my)
Y_upr <- exp(X_upr + my)

# Plot Y with confidence intervals
png("Assignment3/Task2/plots/Y_plot_with_CI.png")
plot(
    exp(X[1:36] + my), 
    type = "l", 
    main = "Plot of generated power with Forecasted Values \n and Confidence Intervals", 
    xlab = "Index", 
    ylab = "Generation (MWh)", 
    col = "blue", 
    lwd = 2,
    xlim = c(0, 48)
)
lines(36:37, c(exp(X[36] + my), Y_lwr[1]), col = "red", lty = 2, lwd = 2)
lines(36:37, c(exp(X[36] + my), Y_upr[1]), col = "red", lty = 2, lwd = 2)
lines(37:length(X), Y_lwr, col = "red", lty = 2, lwd = 2)
polygon(
    c(37:length(X), rev(37:length(X))), 
    c(Y_lwr, rev(Y_upr)), 
    col = rgb(0.2, 0.2, 0.8, 0.5), 
    border = NA
)
lines(37:length(X), Y_upr, col = "red", lty = 2, lwd = 2)
legend(
    "topright", 
    legend = c("Actual Values", "Forecasted Values", "Confidence Interval"), 
    col = c("blue", "red", rgb(0.2, 0.2, 0.8, 0.5)), 
    lty = c(1, 2, NA), 
    lwd = c(2, 2, NA), 
    fill = c(NA, NA, rgb(0.2, 0.2, 0.8, 0.5))
)
dev.off()
Y_diff <- Y_upr - Y_lwr
#max(Y_diff)
