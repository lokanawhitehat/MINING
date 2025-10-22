# Load necessary library
library(splines)

# Define knots
inner_knots <- c(0.2, 0.4, 0.6, 0.8)
boundary_knots <- c(0, 1)

# Define x values for evaluation
x_vals <- seq(0, 1, length.out = 1000)

# Function to plot B-spline basis for different orders M
plot_b_spline_basis <- function(M) {
  # Generate knots with degree + 1 repetitions at boundaries
  knots <- c(rep(boundary_knots[1], M), inner_knots, rep(boundary_knots[2], M))
  
  # Compute B-spline basis
  B <- bs(x_vals, knots = knots, degree = M - 1, intercept = TRUE)
  
  # Plot basis functions
  matplot(x_vals, B, type = "l", lty = 1, lwd = 2, col = rainbow(ncol(B)),
          main = paste("B-spline Basis Functions (M =", M, ")"),
          xlab = "x", ylab = "Basis function value")
  legend("topright", legend = paste("Basis", 1:ncol(B)), col = rainbow(ncol(B)), lty = 1, cex = 0.6)
}

# Generate plots for M = 1 to 5
par(mfrow = c(3, 2))  # Set layout for multiple plots
for (M in 1:5) {
  plot_b_spline_basis(M)
}
