# Load necessary library
library(ggplot2)

# Simulate dataset similar to forensic glass data
set.seed(42)  # For reproducibility
X <- seq(1.51, 1.54, length.out = 100)  # Simulated refractive index
Y <- 5 + 10 * (X - mean(X))^2 + rnorm(100, sd = 0.3)  # Simulated aluminium content with noise

# Fit smoothing splines with different λ values
smooth_loocv <- smooth.spline(X, Y, cv = TRUE)  # LOOCV-selected λ
smooth_lambda_00001 <- smooth.spline(X, Y, lambda = 0.00001)  # Very low λ (overfitting)
smooth_lambda_01 <- smooth.spline(X, Y, lambda = 0.1)  # Moderate λ
smooth_lambda_1 <- smooth.spline(X, Y, lambda = 1)  # High λ but reasonable

# Plot the data points
plot(X, Y, pch = 16, col = "gray", xlab = "Refractive Index (X)", ylab = "Aluminium Content (Y)",
     main = "Smoothing Splines with Different λ Values")

# Add smoothing splines to the plot
lines(smooth_loocv, col = "blue", lwd = 2)  # LOOCV λ
lines(smooth_lambda_00001, col = "red", lwd = 2, lty = 2)  # Very small λ
lines(smooth_lambda_01, col = "green", lwd = 2, lty = 3)  # Moderate λ
lines(smooth_lambda_1, col = "purple", lwd = 2, lty = 4)  # High λ but reasonable

# Add legend
legend("topright", legend = c("Data", "LOOCV λ", "λ = 0.00001", "λ = 0.1", "λ = 1"),
       col = c("gray", "blue", "red", "green", "purple"), pch = c(16, NA, NA, NA, NA), 
       lty = c(NA, 1, 2, 3, 4), lwd = c(NA, 2, 2, 2, 2))

