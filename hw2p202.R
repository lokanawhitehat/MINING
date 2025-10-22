# Load necessary libraries
library(car)

# Load dataset
library(emetricsrsw)
dat <- birthweight_smoking

# Check for collinearity among predictors using alias()
alias_info <- alias(lm(birthweight ~ ., data = dat))
print(alias_info$Complete)  # Identify fully collinear variables

# Remove highly collinear variables manually
dat_clean <- dat[, !colnames(dat) %in% c("tripre0", "tripre1", "tripre2", "tripre3")]

# Fit a linear regression model excluding collinear predictors
model <- lm(birthweight ~ ., data = dat_clean)

# Display model summary
summary(model)

# Check Variance Inflation Factor (VIF) to ensure no multicollinearity
vif_values <- vif(model)
print(vif_values)

# Generate diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Reset plotting layout
par(mfrow = c(1, 1))