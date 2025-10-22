# Load necessary library
library(splines)

# Define the inner and boundary knots
knots <- c(0.2, 0.4, 0.6, 0.8)
boundary_knots <- c(0, 1)

# Define a sequence of x values
x.seq <- seq(0, 1, length.out = 1000)

# Generate B-spline basis for M = 2 (linear splines)
M2_basis <- bs(x.seq, knots = knots, Boundary.knots = boundary_knots, degree = 1, intercept = TRUE)

# Generate B-spline basis for M = 4 (cubic splines)
M4_basis <- bs(x.seq, knots = knots, Boundary.knots = boundary_knots, degree = 3, intercept = TRUE)

# Check the number of basis functions
num_basis_M2 <- ncol(M2_basis)
num_basis_M4 <- ncol(M4_basis)

# Define coefficients dynamically to match basis dimensions
coeffs_M2 <- runif(num_basis_M2, -1, 1)  # Random values between -1 and 1
coeffs_M4 <- runif(num_basis_M4, -1, 1)

# Compute the linear combinations of the basis functions
linear_comb_M2 <- M2_basis %*% coeffs_M2
linear_comb_M4 <- M4_basis %*% coeffs_M4

# Plot the linear combinations
par(mfrow = c(1, 2))  # Arrange plots side by side

# Plot for M = 2
plot(x.seq, linear_comb_M2, type = "l", col = "blue", lwd = 2, 
     main = "Linear Combination of M=2 Basis", xlab = "x", ylab = "Function Value")

# Plot for M = 4
plot(x.seq, linear_comb_M4, type = "l", col = "red", lwd = 2, 
     main = "Linear Combination of M=4 Basis", xlab = "x", ylab = "Function Value")

