# Load necessary libraries
library(emetricsrsw)

# Load dataset
dat <- birthweight_smoking

# Remove collinear variables
dat_clean <- dat[, !colnames(dat) %in% c("tripre0", "tripre1", "tripre2", "tripre3")]

# Fit a linear model
model <- lm(birthweight ~ ., data = dat_clean)

# Display model summary
summary(model)

# Set seed for reproducibility
set.seed(123)

# Obtain residuals and predicted values
res <- resid(model)
y_hat <- fitted(model)

# Number of bootstrap iterations
B <- 100000
boot_coefs <- numeric(B)  # Store bootstrap estimates

# Perform bootstrap resampling
for (b in 1:B) {
  resampled_residuals <- sample(res, length(res), replace = TRUE)
  y_star <- y_hat + resampled_residuals
  boot_model <- lm(y_star ~ ., data = dat_clean)
  boot_coefs[b] <- coef(boot_model)["smoker"]
}

# Compute bootstrap confidence interval
alpha <- 0.05
lower_quantile <- quantile(boot_coefs, alpha / 2, na.rm = TRUE)
upper_quantile <- quantile(boot_coefs, 1 - alpha / 2, na.rm = TRUE)

# Display the bootstrap confidence interval
print(c(lower_quantile, upper_quantile))
