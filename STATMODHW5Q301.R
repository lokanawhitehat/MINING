# Load necessary libraries (if needed)
# install.packages("ggplot2")  # Uncomment if you need to install ggplot2
library(ggplot2)  # For enhanced plotting (optional)

# Example data (replace with your actual data loading code)
# If your data is in a file, use:
# data <- read.csv("your_data_file.csv")
# X <- data$X
# Y <- data$Y

# Simulated example data with some NA and Inf values
set.seed(123)
X <- c(rnorm(90, mean = 0, sd = 1), rep(NA, 5), rep(Inf, 5))  # X with NA and Inf
Y <- c(2 * X[1:90] + rnorm(90, mean = 0, sd = 0.5), rep(NA, 5), rep(Inf, 5))  # Y with NA and Inf

# Check for missing or infinite values
cat("Number of NA values in X:", sum(is.na(X)), "\n")
cat("Number of NaN values in X:", sum(is.nan(X)), "\n")
cat("Number of Inf values in X:", sum(is.infinite(X)), "\n")

cat("Number of NA values in Y:", sum(is.na(Y)), "\n")
cat("Number of NaN values in Y:", sum(is.nan(Y)), "\n")
cat("Number of Inf values in Y:", sum(is.infinite(Y)), "\n")

# Clean the data by removing rows with NA, NaN, or Inf
clean_data <- data.frame(X = X, Y = Y)
clean_data <- clean_data[is.finite(clean_data$X) & is.finite(clean_data$Y), ]
X <- clean_data$X
Y <- clean_data$Y

# Check the cleaned data
cat("Number of rows after cleaning:", nrow(clean_data), "\n")

# Plot the cleaned data
plot(X, Y, 
     main = "Scatter Plot of Y vs X", 
     xlab = "X (Predictor)", 
     ylab = "Y (Response)", 
     pch = 16,  # Use solid circles for points
     col = "blue")  # Color of the points

# Add a grid for better readability
grid()

# Comment on the plot
cat("The scatter plot shows a clear linear trend between X and Y, with some random noise.
    There are no obvious outliers or clusters in the data. The spread of the data appears
    to be consistent across the range of X, suggesting homoscedasticity. The relationship
    between X and Y seems to be linear, but further analysis (e.g., nonparametric regression)
    can help confirm this.")

