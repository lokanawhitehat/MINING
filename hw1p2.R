set.seed(12345)  # Ensures same results every time you run

n <- 1000  # Sample size
beta_0 <- 2  # Intercept
beta_1 <- 3  # True slope value
sigma <- 2   # Standard deviation of noise

X <- runif(n, 0, 10)  # X values from uniform distribution
epsilon <- rnorm(n, mean = 0, sd = sigma)  # Random noise from normal distribution
Y <- beta_0 + beta_1 * X + epsilon  # Generate response variable

model <- lm(Y ~ X)  # Fit the model
summary(model)  # View summary

beta_1_hat <- coef(model)[2]  # Extract estimated slope
cat("Estimated beta_1:", beta_1_hat, "\n")

B <- 1000  # Number of bootstrap samples
beta_1_bootstrap <- numeric(B)  # Store bootstrap estimates

for (i in 1:B) {
  indices <- sample(1:n, n, replace = TRUE)  # Sample with replacement
  X_boot <- X[indices]
  Y_boot <- Y[indices]
  model_boot <- lm(Y_boot ~ X_boot)  # Fit bootstrap model
  beta_1_bootstrap[i] <- coef(model_boot)[2]  # Store bootstrap slope
}

ci_bootstrap <- quantile(beta_1_bootstrap, probs = c(0.025, 0.975))
cat("Bootstrap Confidence Interval for Beta_1:", ci_bootstrap, "\n")


se_beta_1 <- summary(model)$coefficients[2,2]  # Standard error of beta_1
ci_normal <- c(beta_1_hat - 1.96 * se_beta_1, beta_1_hat + 1.96 * se_beta_1)
cat("Normal Approximation Confidence Interval:", ci_normal, "\n")

