# Load necessary library
library(ggplot2)

# Simulate dataset similar to forensic glass data
set.seed(42)  # For reproducibility
X <- seq(1.51, 1.54, length.out = 100)  # Simulated refractive index
Y <- 5 + 10 * (X - mean(X))^2 + rnorm(100, sd = 0.3)  # Simulated aluminium content with noise

# Fit smoothing spline using leave-one-out cross-validation (LOOCV)
smooth_fit <- smooth.spline(X, Y, cv = TRUE)  # LOOCV selects optimal λ

# Plot data points and the smoothing spline
plot(X, Y, pch = 16, col = "gray", xlab = "Refractive Index (X)", ylab = "Aluminium Content (Y)",
     main = "Smoothing Spline Fit (LOOCV λ)")
lines(smooth_fit, col = "blue", lwd = 2)  # Add smoothing spline curve
legend("topright", legend = c("Data", "Smoothing Spline"), col = c("gray", "blue"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
