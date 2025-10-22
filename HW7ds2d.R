# Load necessary libraries
library(ggplot2)
library(splines)

# Simulate dataset similar to forensic glass data
set.seed(42)  # For reproducibility
X <- seq(1.51, 1.54, length.out = 100)  # Simulated refractive index
Y <- 5 + 10 * (X - mean(X))^2 + rnorm(100, sd = 0.3)  # Simulated aluminium content with noise

# Fit a spline regression model with specified degrees of freedom (df=6)
spline_basis <- bs(X, df = 6, degree = 3, intercept = TRUE)
spline_regression <- lm(Y ~ spline_basis)

# Predict values using the fitted model
Y_pred <- predict(spline_regression, newdata = data.frame(spline_basis))

# Retrieve the knots and boundary knots
knots_used <- attr(bs(X, df = 6, degree = 3, intercept = TRUE), "knots")
boundary_knots <- attr(bs(X, df = 6, degree = 3, intercept = TRUE), "Boundary.knots")

# Fit a smoothing spline for comparison
smooth_loocv <- smooth.spline(X, Y, cv = TRUE)

# Plot the data points
plot(X, Y, pch = 16, col = "gray", xlab = "Refractive Index (X)", ylab = "Aluminium Content (Y)",
     main = "Spline Regression (df=6) vs Smoothing Spline")

# Add smoothing spline (LOOCV)
lines(smooth_loocv, col = "blue", lwd = 2)  

# Add fitted spline regression model (df=6)
lines(X, Y_pred, col = "orange", lwd = 2, lty = 2)  

# Add legend
legend("topright", legend = c("Data", "LOOCV Spline", "Spline Regression (df=6)"),
       col = c("gray", "blue", "orange"), 
       pch = c(16, NA, NA), 
       lty = c(NA, 1, 2), 
       lwd = c(NA, 2, 2))

# Print retrieved knots and boundary knots
print("Knots used in spline basis:")
print(knots_used)
print("Boundary knots:")
print(boundary_knots)
