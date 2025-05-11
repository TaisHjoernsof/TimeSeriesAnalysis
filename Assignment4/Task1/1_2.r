# Import the 1_1.r file
source("Assignment4/Task1/1_1.r")
library(ggplot2)

set.seed(1312)
sigma_2 <- 1

e_2 <- rnorm(n, mean = 0, sd = sigma_2)
# Generate Y
Y <- X[1,] + e_2