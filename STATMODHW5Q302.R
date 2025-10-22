# Load necessary libraries
library(np)

# Create a data frame for the np package
data <- data.frame(X = X, Y = Y)

# Fit nonparametric regression models with different bandwidths
# Bandwidth selection is done automatically by npreg()
fit_h1 <- npreg(Y ~ X, data = data, bws = 0.1)  # Bandwidth = 0.1
fit_h2 <- npreg(Y ~ X, data = data, bws = 0.5)  # Bandwidth = 0.5
fit_h3 <- npreg(Y ~ X, data = data, bws = 1)    # Bandwidth = 1

# Define evaluation points (e.g., a sequence of X values)
eval_pts <- seq(min(X), max(X), length.out = 100)

# Predict the regression function at the evaluation points
pred_h1 <- predict(fit_h1, newdata = data.frame(X = eval_pts))
pred_h2 <- predict(fit_h2, newdata = data.frame(X = eval_pts))
pred_h3 <- predict(fit_h3, newdata = data.frame(X = eval_pts))

# Plot the data
plot(X, Y, 
     main = "Scatter Plot of Y vs X with Nonparametric Regression Lines", 
     xlab = "X (Predictor)", 
     ylab = "Y (Response)", 
     pch = 16, 
     col = "blue")

# Add the estimated regression lines
lines(eval_pts, pred_h1, col = "red", lwd = 2, lty = 1)  # h = 0.1
lines(eval_pts, pred_h2, col = "green", lwd = 2, lty = 2)  # h = 0.5
lines(eval_pts, pred_h3, col = "purple", lwd = 2, lty = 3)  # h = 1

# Add a legend
legend("topleft", 
       legend = c("h = 0.1", "h = 0.5", "h = 1"), 
       col = c("red", "green", "purple"), 
       lwd = 2, 
       lty = c(1, 2, 3))