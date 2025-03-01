source("code/read_data.R")


y = Dtrain$total

X = cbind(1, Dtrain$year)

# Task 4.2

p <- ncol(X)
N <- nrow(X)
R <- diag(0.1, p)
theta <- c(0,0)
Theta <- matrix(NA, nrow=N, ncol=p)


# Iterate through and estimate the parameters
for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

# Task 4.3
Theta_OLS <- matrix(NA, nrow=N, ncol=p)
for(i in 1:N){
    m_OLS <- lm(total ~ year, data = Dtrain[1:i,])
    Theta_OLS[i, ] <- coef(m_OLS)
}

print(Theta_OLS[N,])
print(Theta[N,])

# Now checking for better initial values <3

theta = (Theta_OLS[N,])

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta_OLS[N,])
print(Theta[N,])

#Task 4.4

R <- diag(0.1, p)
theta <- c(0,0)
lambda <- 0.7

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- lambda * R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta[N,])
#Plot Theta[,1] and Theta[,2] against time
plot(Dtrain$year, Theta[,1], type = "l", col = "red", xlab = "Year", ylab = "Theta_1")
lines(Dtrain$year, Theta[,2], col = "blue")
legend("topright", legend = c("Theta_1", "Theta_2"), col = c("red", "blue"), lty = 1)
#Save the plot to file
dev.copy(png, "plots/4.4-lambda-0.7.png")
dev.off()


R <- diag(0.1, p)
theta <- c(0,0)
lambda <- 0.99

for(i in 1:N){
  (x <- X[i, ])
  # Update
  (R <- lambda * R + x %*% t(x))
  (theta <- theta + solve(R) %*% x %*% (y[i] - t(x) %*% theta))
  Theta[i, ] <- theta
}

print(Theta[N,])
plot(Dtrain$year, Theta[,1], type = "l", col = "red", xlab = "Year", ylab = "Theta_1")
lines(Dtrain$year, Theta[,2], col = "blue")
legend("topright", legend = c("Theta_1", "Theta_2"), col = c("red", "blue"), lty = 1)
#Save the plot to file
dev.copy(png, "plots/4.4-lambda-0.99.png")
dev.off()