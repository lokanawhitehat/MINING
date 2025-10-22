# Load the necessary library
library(splines)

# Define the inner knots and boundary knots
knots <- c(0.2, 0.4, 0.6, 0.8)
boundary_knots <- c(0, 1)

# Define the grid of values in [0, 1]
x.seq <- seq(0, 1, length.out = 1000)

# Set up the plotting area to display all plots in a 3x2 grid
par(mfrow = c(3, 2))

# Loop through each order M from 1 to 5
for (M in 1:5) {
  if (M == 1) {
    # For M=1, B-spline basis is just step functions
    B <- outer(x.seq, knots, function(x, k) as.numeric(x >= k))
    B <- cbind(as.numeric(x.seq >= boundary_knots[1]), B, as.numeric(x.seq <= boundary_knots[2]))
  } else {
    # For M > 1, use the bs function to generate the B-spline basis
    B <- bs(x.seq, knots = knots, Boundary.knots = boundary_knots, degree = M - 1, intercept = TRUE)
  }
  
  # Plot the basis functions
  matplot(x.seq, B, type = "l", lty = 1, col = 1:ncol(B), 
          main = paste("B-spline Basis of Order", M), 
          xlab = "x", ylab = "Basis Function Value")
  legend("topright", legend = 1:ncol(B), col = 1:ncol(B), lty = 1, cex = 0.8)
}