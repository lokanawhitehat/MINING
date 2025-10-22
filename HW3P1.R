# Load necessary libraries
library(kernlab)
library(ggplot2)

# Load the spam dataset
data(spam)

# Ensure 'type' is a factor
spam$type <- as.factor(spam$type)

# Scatter plot of 'money' vs 'charDollar' labeled by 'type'
ggplot(spam, aes(x = money, y = charDollar, color = type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Money vs CharDollar",
       x = "Money",
       y = "CharDollar",
       color = "Email Type") +
  theme_minimal()

# Fit logistic regression model
fit <- glm(type ~ ., data = spam, family = binomial)

# Compute predictions
prob_pred <- predict(fit, type = "response")
logit_pred <- predict(fit, type = "link")

# Check expit transformation
expit <- function(x) 1 / (1 + exp(-x))
all.equal(expit(logit_pred), prob_pred)  # Should return TRUE

# Compute 95% confidence intervals using Normal approximation
conf_intervals <- confint.default(fit)
print(conf_intervals)

# Compute predicted probabilities
pred_prob <- predict(fit, type = "response")

# Compute summation for three covariates
sum_money <- sum(spam$money * (as.numeric(spam$type) - pred_prob))
sum_charDollar <- sum(spam$charDollar * (as.numeric(spam$type) - pred_prob))
sum_make <- sum(spam$make * (as.numeric(spam$type) - pred_prob))

# Print results
cat("Sum for 'money':", sum_money, "\n")
cat("Sum for 'charDollar':", sum_charDollar, "\n")
cat("Sum for 'make':", sum_make, "\n")

# Compute design matrix and variance-covariance matrix
design_mat <- model.matrix(fit)
predicted_probs <- predict(fit, type = "response")
W_mat <- diag(predicted_probs * (1 - predicted_probs))
V_mat <- solve(t(design_mat) %*% W_mat %*% design_mat)

# Select index for coefficient
idx_values <- c(2, 3, 4)  # Three different predictors

for (idx in idx_values) {
  manual_CI <- coef(fit)[idx] + c(-1, 1) * qnorm(0.975) * sqrt(V_mat[idx, idx])
  glm_CI <- confint.default(fit)[idx, ]
  
  cat("Manual 95% CI for Beta_", idx, ":", manual_CI, "\n")
  cat("confint.default() 95% CI for Beta_", idx, ":", glm_CI, "\n\n")
}

# Compute median feature values
x0 <- apply(model.matrix(fit), 2, quantile, p = 0.5)

# Compute linear predictor x0'
linear_pred <- sum(x0 * coef(fit))

# Compute predicted probability using expit function
predicted_prob <- expit(linear_pred)

# Print result
cat("Predicted probability of spam for median feature values:", predicted_prob, "\n")

# Compute variance of x0'
var_x0_beta <- as.numeric(t(x0) %*% V_mat %*% x0)
se_x0_beta <- sqrt(var_x0_beta)

# Compute 95% confidence interval for x0'
z_value <- qnorm(0.975)
ci_x0_beta <- linear_pred + c(-1, 1) * z_value * as.numeric(se_x0_beta)

# Compute confidence interval for the predicted probability
ci_pred_prob <- expit(ci_x0_beta)

# Print final result
cat("95% Confidence Interval for predicted probability:", ci_pred_prob, "\n")
