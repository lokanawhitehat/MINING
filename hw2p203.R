# Install necessary packages if not already installed
install.packages("sandwich")  # For robust standard errors
install.packages("lmtest")  # For hypothesis testing

# Load required libraries
library(sandwich)
library(lmtest)
library(car)

# Load dataset
library(emetricsrsw)
dat <- birthweight_smoking

# Remove collinear tripre variables
dat_clean <- dat[, !colnames(dat) %in% c("tripre0", "tripre1", "tripre2", "tripre3")]

# Fit a linear model excluding collinear predictors
model <- lm(birthweight ~ ., data = dat_clean)

# Display model summary
summary(model)

# Compute standard confidence intervals under homoscedasticity
std_ci <- confint(model, "smoker", level = 0.95)
print("Confidence Interval (Standard Homoscedastic Model):")
print(std_ci)

# Compute sandwich estimator for robust standard errors
sandwich_se <- sqrt(diag(vcovHC(model, type = "HC")))

# Compute robust confidence interval for smoking coefficient
smoking_coef <- coef(model)["smoker"]
smoking_se <- sandwich_se["smoker"]
robust_ci <- smoking_coef + c(-1, 1) * qnorm(0.975) * smoking_se

print("Confidence Interval (Heteroscedasticity-Robust - Sandwich Estimator):")
print(robust_ci)