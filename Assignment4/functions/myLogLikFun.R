source("Assignment4/functions/kalmanfilter.R")
source("Assignment4/functions/generate_ssm.r")

myLogLikFun <- function(theta, y, R, x_prior = 0, P_prior = 10) {
  a <- theta[1]  # Extract parameters from theta
  b <- theta[2]
  c <- abs(theta[3])
  kf_result <- myKalmanFilter(y,c(a,b,c),R,x_prior,P_prior) # call the Kalman filter function
  err <- kf_result$innovation       # Innovations
  S <- kf_result$innovation_var   # Innovation covariances
  
  # Compute log-likelihood contributions from each time step
  logL <- -0.5 * sum(log(S) + (err^2)/S)
  return(-logL)  # Return negative log-likelihood for minimization
}