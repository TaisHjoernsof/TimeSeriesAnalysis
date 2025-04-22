sim <- function(model, n, nburnin=100){
  n <- n + nburnin
  # Take the ar and ma part
  ar <- model$ar
  ma <- model$ma
  # The order (i.e. the number of lags)
  p <- length(ar)
  q <- length(ma)
  # The vector for the simulation result
  y <- numeric(n)
  # Generate the random normal values
  eps <- rnorm(n)
  # Run the simulation
  for(i in (max(p,q)+1):n){
    y[i] <- eps[i] + sum(ar * y[i-(1:p)]) + sum(ma * eps[i-(1:q)])
  }
  # Return without the burn-in period
  return(y[(nburnin+1):n])
}

#Function to generate ACF data directly from ARMA model
generate_acf <- function(model, lag.max=30){
    # Extract AR and MA coefficients
    ar <- model$ar
    ma <- model$ma
    # Compute and return the theoretical ACF
    return(ARMAacf(ar=ar, ma=ma, lag.max=lag.max, pacf=FALSE))
}

lagvec <- function(x, lag){
    if (lag > 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(rep(NA, lag), x[1:(length(x) - lag)]))
    }else if(lag < 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(x[(abs(lag) + 1):length(x)], rep(NA, abs(lag))))
    }else{
        ## lag = 0, return x
        return(x)
    }
}

rho <- function(k, phi_1, phi_2) {
  rho <- list()

  rho[[1]] <- 1
  rho[[2]] <- -(phi_1 / (1 + phi_2))
  rho[[3]] <- -(rho[[2]] * phi_1 + phi_2)

  for (i in 4:(k + 1)) {
    rho[[i]] <- -(rho[[i - 1]] * phi_1 + rho[[i - 2]] * phi_2)
  }
  return(rho)
}
