# Define any square matrix (change values as needed)
R <- matrix(c(0.1, 201.8, 201.8, 407232.4), nrow = 2, byrow = TRUE)
# Load MASS package for pseudoinverse function
library(MASS)
# Compute determinant
det_R <- det(R)

# If the matrix is nearly singular, use pseudoinverse
if (abs(det_R) > 1e-10) {
  R_inv <- solve(R)  # Standard inverse
  cat("Standard Inverse of R:\n")
} else {
  R_inv <- ginv(R)  # Moore-Penrose Pseudoinverse
  cat("Matrix is nearly singular. Using Pseudoinverse:\n")
}

# Print results
print(R_inv)

# Definiere die resultierende 2x2 Matrix aus der vorherigen Berechnung
M <- matrix(c(0.000054, 0.144, -0.00000512, -0.009946), nrow = 2, byrow = TRUE)

# Berechne die Determinante
det_M <- det(M)

# Überprüfen, ob die Matrix invertierbar ist
if (abs(det_M) > 1e-10) {  # Vermeidung numerischer Instabilität
  M_inv <- solve(M)  # Berechnung der Inversen
  cat("Inverse der Matrix M:\n")
  print(M_inv)
} else {
  cat("Die Matrix ist singulär und nicht invertierbar.")
}

# Definiere die Werte
theta_t_minus1 <- matrix(c(-27.5, -0.0000152), ncol = 1)
x_t <- matrix(c(1, 2018.083), ncol = 1)
Y_t <- 2.934
x_t_T <- t(x_t)

# Berechnung von x_t^T * theta_t_minus1
x_t_theta <- x_t_T %*% theta_t_minus1

# Fehler berechnen
e_t <- Y_t - x_t_theta

# Berechnung von x_t * e_t
x_t_e_t <- x_t * e_t

# Definiere R_t^{-1} aus vorheriger Berechnung
R_t_inv <- matrix(c(-49865.7, -719353.5, 25.6, 269.7), nrow = 2, byrow = TRUE)

# Berechnung von R_t^{-1} * x_t * e_t
update_term <- R_t_inv %*% x_t_e_t

# Neues Theta berechnen
theta_t <- theta_t_minus1 + update_term

# Ausgabe
cat("Neues Theta_t:\n")
print(theta_t)
