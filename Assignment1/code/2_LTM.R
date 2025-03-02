library(xtable)
source("Assignment1/code/read_data.R")

y <- Dtrain$total[1:3]
x <- Dtrain$year[1:3]
m <- lm(y ~ x) #Note that glm and lm give different results when using the predict function with interval = "prediction"

# Plot the estimates of y using m with the data as points in a square plot
png("Assignment 1/graphs/2.1-line.png")
plot(x, y, pch = 19, col = "blue")
abline(m, col = "red")
dev.off()

# Predict the value of y for x-range (2018+4/12 to 2019+4/12)
x_new <- seq(2018 + 3 / 12, 2019 + 2 / 12, 1 / 12)
y_new <- predict(m, data.frame(x = x_new), type = "response")

# Round x_new and y_new to 3 decimals
x_new <- round(x_new, 3)
y_new <- round(y_new, 3)


# Write x_new and y_new to a csv file
write.csv(data.frame(x = x_new, y = y_new), file = "Assignment 1/output/2.1.csv", row.names = FALSE)

# Create prediction intervals of level 0.95 for y_hat
p_i <- predict(m, newdata = data.frame(x = Dtrain$year[1:(3 + 12)]), interval = "prediction", level = 0.95)

png("Assignment 1/graphs/2.1-prediction.png")
plot(Dtrain$year[1:(3 + 12)], Dtrain$total[1:(3 + 12)],ylim = c(min(p_i[,2]),max(p_i[,3])), pch = 19, col = "blue") # Actual data
points(Dtrain$year[1:(3 + 12)], p_i[, 1], pch = 19, col = "red")                # Predicted data
abline(m, col = "red")                                                          # Regression line
lines(Dtrain$year[1:(3 + 12)], p_i[, 2], col = "green")                         # Lower prediction interval
lines(Dtrain$year[1:(3 + 12)], p_i[, 3], col = "green")                         # Upper prediction interval
dev.off()

png("Assignment 1/graphs/2.1-qq.png")
qqnorm(Dtrain$total[1:(3 + 12)])
qqline(Dtrain$total[1:(3 + 12)], col = "red")
dev.off()