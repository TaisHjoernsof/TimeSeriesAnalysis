kf_logLik_dt <- function(par, df) {
  # Unpack parameters
  A <- par[1]
  B <- matrix(par[2:4], nrow = 1)  # 1x3 input matrix
  q <- par[5]                      # system noise variance
  r <- par[6]                      # observation noise variance
  x0 <- matrix(par[7], nrow = 1)   # initial state estimate
  
  # Observation matrix
  C <- matrix(1, nrow = 1)
  
  # Observation and input data
  Y <- as.matrix(df$Y)
  U <- as.matrix(df[, c("Ta", "S", "I")])
  Tn <- nrow(Y)
  
  # Initialize estimates
  x_est <- x0
  P_est <- matrix(10)  # initial state covariance
  logLik <- 0
  
  for (t in 1:Tn) {
    # Prediction
    x_pred <- A * x_est + B %*% matrix(U[t, ], ncol = 1)
    P_pred <- A * P_est * A + q
    
    # Innovation (observation residual)
    y_pred <- C %*% x_pred
    S_t <- C %*% P_pred %*% t(C) + r
    innov <- Y[t] - y_pred
    
    # Log-likelihood contribution
    if (S_t <= 0 || !is.finite(S_t)) return(-1e6)  # bail out
    logLik <- logLik - 0.5 * (log(2 * pi * S_t) + (innov^2) / S_t)
    
    # Kalman gain
    K_t <- P_pred %*% t(C) %*% solve(S_t)
    
    # Update step
    x_est <- x_pred + K_t %*% innov
    P_est <- (1 - K_t %*% C) * P_pred
  }
  
  return(as.numeric(logLik))
}


kf_logLik_dt_2d <- function(par, df) {
  # Unpack parameters
  A <- matrix(par[1:4], nrow = 2, byrow = TRUE)
  B <- matrix(par[5:10], nrow = 2, byrow = TRUE)
  Q_lt <- matrix(c(par[11], 0, par[12], par[13]), nrow = 2, byrow = TRUE)
  Q <- Q_lt %*% t(Q_lt)
  
  R <- par[14]                                # Observation noise variance
  X0 <- matrix(par[15:16], nrow = 2)          # Estimated initial state
  C <- matrix(c(1, 0), nrow = 1)              # Only observe first state
  
  # Extract data
  Y <- as.matrix(df$Y)
  U <- as.matrix(df[, c("Ta", "S", "I")])
  Tn <- nrow(Y)
  
  # Init
  x_est <- X0
  P_est <- diag(10, 2)
  logLik <- 0
  
  for (t in 1:Tn) {
    u_t <- matrix(U[t, ], ncol = 1)
    x_pred <- A %*% x_est + B %*% u_t
    P_pred <- A %*% P_est %*% t(A) + Q
    
    y_pred <- C %*% x_pred
    S_t <- C %*% P_pred %*% t(C) + R
    
    if (S_t <= 0 || !is.finite(S_t)) return(-1e6)  # Bail out on numerical issues
    
    innov <- Y[t] - y_pred
    logLik <- logLik - 0.5 * (log(2 * pi * S_t) + (innov^2) / S_t)
    
    # Kalman update
    K_t <- P_pred %*% t(C) %*% solve(S_t)
    x_est <- x_pred + K_t %*% innov
    P_est <- (diag(2) - K_t %*% C) %*% P_pred
  }
  
  return(as.numeric(logLik))
}



# Optimizer wrapper
estimate_dt <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par){ -kf_logLik_dt(par, df) }
  optim(
    par    = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower  = lower, upper = upper,
    control= list(maxit=1000, trace=1)
  )
}

estimate_dt_2d <- function(start_par, df, lower = NULL, upper = NULL) {
  negLL <- function(par) {
    -kf_logLik_dt_2d(par, df)  # must be the updated version using X0
  }
  
  optim(
    par     = start_par,
    fn      = negLL,
    method  = "L-BFGS-B",
    lower   = lower,
    upper   = upper,
    control = list(maxit = 1000, trace = 1)
  )
}

