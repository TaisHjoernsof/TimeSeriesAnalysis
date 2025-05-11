source("Assignment4/functions/kalmanfilter.R")
source("Assignment4/Task1/1_2.r")

kalman <- myKalmanFilter(Y, theta = c(a, b, sigma_1), R = sigma_2, x_prior = X_0)

z_95 <- qnorm(0.975)

x_pred_upper <- kalman$x_pred + z_95 * sqrt(kalman$P_pred)
x_pred_lower <- kalman$x_pred - z_95 * sqrt(kalman$P_pred)
