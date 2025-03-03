library(xtable)
source("code/read_data.R")

y <- Dtrain$total[1:3]
x <- Dtrain$year[1:3]
m <- lm(y ~ x) #Note that glm and lm give different results when using the predict function with interval = "prediction"

# Plot the estimates of y using m with the data as points in a square plot
png("plots/2.1-line.png")
plot(x, y, pch = 19, col = "blue")
abline(m, col = "red")
dev.off()

# Predict the value of y for x-range (2018+4/12 to 2019+4/12)
x_new <- seq(2018 + 3 / 12, 2019 + 2 / 12, 1 / 12)
y_new <- predict(m, newdata = data.frame(x = Dtrain$year[1:(3 + 12)]), interval = "prediction", level = 0.95)

# Round x_new and y_new to 3 decimals
x_new <- round(x_new, 3)
y_new <- round(y_new, 3)


# Add x_new as a column called x to the data frame y_new
y_new <- cbind(y_new, x_new)

# Move x_new to the first column and rename to yea
y_new <- y_new[, c(4, 2, 1, 3)]
colnames(y_new) <- c("year", "lwr", "fit", "upr")

write.csv(y_new, "data/2.1-prediction.csv", row.names = FALSE)


png("plots/2.1-prediction.png")
plot(Dtrain$year[1:(3 + 12)], Dtrain$total[1:(3 + 12)],ylim = c(min(y_new[,2]),max(y_new[,3])), pch = 19, col = "blue") # Actual data
points(Dtrain$year[1:(3 + 12)], y_new[, 1], pch = 19, col = "red")                # Predicted data
abline(m, col = "red")                                                          # Regression line
lines(Dtrain$year[1:(3 + 12)], y_new[, 2], col = "green")                         # Lower prediction interval
lines(Dtrain$year[1:(3 + 12)], y_new[, 3], col = "green")                         # Upper prediction interval
dev.off()

png("plots/2.1-qq.png")
qqnorm(Dtrain$total[1:(3 + 12)])
qqline(Dtrain$total[1:(3 + 12)], col = "red")
dev.off()