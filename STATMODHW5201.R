# Install and load the 'pls' package (if not already installed)
if (!require(pls)) install.packages("pls")
library(pls)

# Load the gasoline dataset
data(gasoline)

# Create the vector of outcomes (Y) and the matrix of predictors (X)
X <- as.matrix(gasoline$NIR)  # NIR spectra (predictors)
Y <- gasoline$octane          # Octane (response)

# Convert X to a data frame and assign column names
X_df <- as.data.frame(X)
colnames(X_df) <- paste0("X", 1:ncol(X_df))  # Assign generic column names (X1, X2, ..., Xp)

# Fit a linear regression model
lm_model <- lm(Y ~ ., data = X_df)  # Use '.' to include all columns in X_df

# Summary of the model
summary(lm_model)

# Residual sum of squares (RSS)
RSS <- sum(resid(lm_model)^2)
print(RSS)  # Print the RSS value


# Check dimensions of X and Y
dim(X)
length(Y)

# Check the rank of X
rankMatrix(X)

# Install and load the glmnet package
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Fit a ridge regression model (alpha = 0)
ridge_model <- glmnet(X, Y, alpha = 0, lambda = 15)

# Coefficients
ridge_coef <- coef(ridge_model)
print(ridge_coef)

# Residual sum of squares (RSS)
ridge_predictions <- predict(ridge_model, X)
RSS_ridge <- sum((Y - ridge_predictions)^2)
print(RSS_ridge)

# Fit a PCR model
pcr_model <- pcr(Y ~ X, scale = TRUE, validation = "CV")

# Summary of the model
summary(pcr_model)

# Residual sum of squares (RSS)
pcr_predictions <- predict(pcr_model, X)
RSS_pcr <- sum((Y - pcr_predictions)^2)
print(RSS_pcr)