install.packages("splines")
library(splines)
# Load necessary library


# Define knots
inner_knots <- c(0.2, 0.4, 0.6, 0.8)
boundary_knots <- c(0, 1)
x.seq <- seq(0, 1, length.out = 1000)

# Function to plot B-splines for different M
plot_b_splines <- function(M) {
  # Compute B-spline basis
  if (M == 1) {
    # Linear splines (piecewise linear functions)
    B <- bs(x.seq, knots = inner_knots, Boundary.knots = boundary_knots, degree = 0, intercept = TRUE)
  } else {
    B <- bs(x.seq, knots = inner_knots, Boundary.knots = boundary_knots, degree = M-1, intercept = TRUE)
  }
  
  # Plot the basis functions
  matplot(x.seq, B, type = "l", lwd = 2, lty = 1, col = rainbow(ncol(B)),
          main = paste("B-spline basis of order M =", M),
          xlab = "x", ylab = "Basis functions")
  legend("topright", legend = paste("Basis", 1:ncol(B)), col = rainbow(ncol(B)), lwd = 2)
}

# Generate plots for M = 1 to 5
par(mfrow = c(3, 2))  # Arrange plots in a grid
for (M in 1:5) {
  plot_b_splines(M)
}

