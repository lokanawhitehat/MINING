# Fit nonparametric regression with automatic bandwidth selection
fit_auto <- npreg(Y ~ X, data = data)

# Predict the regression function at the evaluation points
pred_auto <- predict(fit_auto, newdata = data.frame(X = eval_pts))

# Plot the data
plot(X, Y, 
     main = "Scatter Plot of Y vs X with Automatic Bandwidth Selection", 
     xlab = "X (Predictor)", 
     ylab = "Y (Response)", 
     pch = 16, 
     col = "blue")

# Add the estimated regression line
lines(eval_pts, pred_auto, col = "orange", lwd = 2)

# Add a legend
legend("topleft", 
       legend = "Automatic Bandwidth", 
       col = "orange", 
       lwd = 2)