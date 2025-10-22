# Load necessary libraries
if (!require(pls)) install.packages("pls")  # Install the 'pls' package if not already installed
library(pls)  # Load the 'pls' package to access the gasoline dataset

if (!require(glmnet)) install.packages("glmnet")  # Install the 'glmnet' package if not already installed
library(glmnet)  # Load the 'glmnet' package for ridge regression

# Load the gasoline dataset
data(gasoline)

# Create the matrix of predictors (X) and the vector of outcomes (Y)
X <- as.matrix(gasoline$NIR)  # NIR spectra (predictors)
Y <- gasoline$octane           # Octane (response)

# Fit a ridge regression model with lambda = 15
ridge_model <- glmnet(X, Y, alpha = 0, lambda = 15)

# Extract the coefficients
ridge_coef <- coef(ridge_model)

# Print the coefficients
print(ridge_coef)

# Number of non-zero coefficients (excluding the intercept)
num_non_zero <- sum(ridge_coef[-1] != 0)  # Exclude the intercept
print(paste("Number of non-zero coefficients:", num_non_zero))

# Values of the first 5 coefficient estimates (excluding the intercept)
first_five_coef <- ridge_coef[2:6]  # First 5 coefficients after the intercept
print("First 5 coefficient estimates:")
print(first_five_coef)

# Compute the norm of the coefficients (excluding the intercept)
beta_norm <- sqrt(sum(ridge_coef[-1]^2))  # L2 norm of the coefficients
print(paste("Norm of the coefficients (||β||):", beta_norm))

# Repeat the process with lambda = 100
ridge_model_lambda100 <- glmnet(X, Y, alpha = 0, lambda = 100)
ridge_coef_lambda100 <- coef(ridge_model_lambda100)

# Number of non-zero coefficients with lambda = 100
num_non_zero_lambda100 <- sum(ridge_coef_lambda100[-1] != 0)
print(paste("Number of non-zero coefficients (lambda = 100):", num_non_zero_lambda100))

# Norm of the coefficients with lambda = 100
beta_norm_lambda100 <- sqrt(sum(ridge_coef_lambda100[-1]^2))
print(paste("Norm of the coefficients (lambda = 100, ||β||):", beta_norm_lambda100))