source("code/read_data.R")


y = Dtrain$total

X = cbind(1, Dtrain$year)

# Task 4.2

p <- ncol(X)
N <- nrow(X)
R <- diag(0.1, p)
theta <- c(0,0)#rep(0, p)
Theta <- matrix(NA, nrow=N, ncol=p)


# Iterate through and estimate the parameters
for(i in 1:3){
  (x <- X[i, ])
  # Update
  (R <- R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

