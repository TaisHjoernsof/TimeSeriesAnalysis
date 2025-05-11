source("Assignment4/functions/myLogLikFun.R")
source("Assignment4/functions/generate_ssm.r")



X_0 <- 5
n <- 100
m <- 100
sigma_2 <- 1

set.seed(1312)

optimize_theta <- function(initial_theta) {
    theta_opt <- matrix(0, nrow = m, ncol = 3)
    a <- initial_theta[1]
    b <- initial_theta[2]
    c <- initial_theta[3]
    for (i in 1:m) {
        x <- generate_X(a, b, c, X_0, n)
        y <- generate_Y(x, sigma_2)

        result <- optim(
            par = initial_theta,
            y = y,
            R = sigma_2,
            x_prior = X_0,
            P_prior = 10,
            fn = myLogLikFun,
            method = "BFGS",
            control = list(fnscale = 1)
        )
        theta_opt[i, ] <- result$par
        print(i)
    }
    return(theta_opt)
}

theta_opt_1 <- optimize_theta(c(1, 0.9, 1))
theta_opt_2 <- optimize_theta(c(5, 0.9, 1))
theta_opt_3 <- optimize_theta(c(1, 0.9, 5))
