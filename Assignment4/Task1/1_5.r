source("Assignment4/functions/myLogLikFun.R")
source("Assignment4/functions/generate_ssm.r")

nu <- c(100,5,2,1)
X_0 <- 5
n <- 100
m <- 100
sigma_2 <- 1



optimize_theta_with_t_dist <- function(initial_theta, nu) {
    theta_opt <- matrix(0, nrow = m, ncol = 3)
    a <- initial_theta[1]
    b <- initial_theta[2]
    c <- initial_theta[3]
    for (i in 1:m) {
        x <- generate_X_with_student_t_dist(a,b,c,X_0,n,nu)
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

theta_opt_1 <- optimize_theta_with_t_dist(c(1, 0.9, 1), nu[1])
theta_opt_2 <- optimize_theta_with_t_dist(c(1, 0.9, 1), nu[2])
theta_opt_3 <- optimize_theta_with_t_dist(c(1, 0.9, 1), nu[3])
theta_opt_4 <- optimize_theta_with_t_dist(c(1, 0.9, 1), nu[4])