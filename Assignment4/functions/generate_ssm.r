generate_X <- function(a, b, sigma_1, X_0, n) {
    e_1 <- matrix(rnorm(n, mean = 0, sd = abs(sigma_1)))
    X <- numeric(n)

    X[1] <- a * X_0 + b + e_1[1]

    for (i in 2:n) {
        X[i] <- a * X[i - 1] + b + e_1[i]
    }
    return(X)
}

generate_X_with_student_t_dist <- function(a, b, sigma_1, X_0, n, nu) {
    e_1 <- matrix(rt(n, df = nu))
    X <- numeric(n)

    X[1] <- a * X_0 + b + e_1[1]

    for (i in 2:n) {
        X[i] <- a * X[i - 1] + b + e_1[i]
    }
    return(X)
}


generate_Y <- function(X, sigma_2) {
    n <- length(X)
    e_2 <- rnorm(n, sd = sigma_2)
    y <- X + e_2
    return(y)
}
