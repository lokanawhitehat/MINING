# Load necessary libraries
if (!require(pls)) install.packages("pls")  # Install the 'pls' package if not already installed
library(pls)  # Load the 'pls' package to access the gasoline dataset

if (!require(glmnet)) install.packages("glmnet")  # Install the 'glmnet' package if not already installed
library(glmnet)  # Load the 'glmnet' package for LASSO regression

# Load the gasoline dataset
data(gasoline)

# Create the matrix of predictors (X) and the vector of outcomes (Y)
X <- as.matrix(gasoline$NIR)  # NIR spectra (predictors)
Y <- gasoline$octane           # Octane (response)

# Fit a LASSO regression model with lambda = 0.03
lasso_model <- glmnet(X, Y, alpha = 1, lambda = 0.03)

# Extract the coefficients
lasso_coef <- coef(lasso_model)

# Print the coefficients
print(lasso_coef)

# Number of non-zero coefficients (excluding the intercept)
num_non_zero <- sum(lasso_coef[-1] != 0)  # Exclude the intercept
print(paste("Number of non-zero coefficients (lambda = 0.03):", num_non_zero))

# Values of the first 5 coefficient estimates (excluding the intercept)
first_five_coef <- lasso_coef[2:6]  # First 5 coefficients after the intercept
print("First 5 coefficient estimates (lambda = 0.03):")
print(first_five_coef)

# Compute the norm of the coefficients (excluding the intercept)
beta_norm <- sqrt(sum(lasso_coef[-1]^2))  # L1 norm of the coefficients
print(paste("Norm of the coefficients (lambda = 0.03, ||β||):", beta_norm))

# Repeat the process with lambda = 0.5
lasso_model_lambda05 <- glmnet(X, Y, alpha = 1, lambda = 0.5)
lasso_coef_lambda05 <- coef(lasso_model_lambda05)

# Number of non-zero coefficients with lambda = 0.5
num_non_zero_lambda05 <- sum(lasso_coef_lambda05[-1] != 0)
print(paste("Number of non-zero coefficients (lambda = 0.5):", num_non_zero_lambda05))

# Norm of the coefficients with lambda = 0.5
beta_norm_lambda05 <- sqrt(sum(lasso_coef_lambda05[-1]^2))
print(paste("Norm of the coefficients (lambda = 0.5, ||β||):", beta_norm_lambda05))