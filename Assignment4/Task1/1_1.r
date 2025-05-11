a <- 0.9
b <- 1
sigma_1 <- 1
X_0 <- 5
n <- 100
m <- 5

set.seed(1312)

e_1 <- matrix(rnorm(m * n, mean = 0, sd = sigma_1), nrow = m, ncol = n)
X <- matrix(0, nrow = m, ncol = n)

X[,1] <- a * X_0 + b + e_1[,1]

for (i in 2:n) {
    X[,i] <- a * X[,i-1] + b + e_1[,i]
}