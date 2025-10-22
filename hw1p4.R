set.seed(12345)

n <- 1000  # Sample size
B <- 1000  # Number of bootstrap samples

X <- runif(n, 0, 1)  # Generate original sample
X_n <- max(X)  # Compute sample maximum

X_n_bootstrap <- numeric(B)  # Store bootstrap maxima

for (i in 1:B) {
  X_star <- sample(X, n, replace = TRUE)  # Bootstrap resampling
  X_n_bootstrap[i] <- max(X_star)  # Compute max for each bootstrap sample
}

prob_max_equals <- mean(X_n_bootstrap == X_n)
cat("Empirical Probability P(X_n* = X_n):", prob_max_equals, "\n")

expected_prob <- 1 - exp(-1)  # 1 - e^-1
cat("Theoretical Probability (1 - e^-1):", expected_prob, "\n")

hist(n * (1 - X_n_bootstrap), 
     breaks = 50, 
     probability = TRUE, 
     main = "Bootstrap Distribution of n(1 - X_n*)",
     xlab = "n(1 - X_n*)", 
     col = "lightblue", 
     border = "black")

# Add theoretical density curve
curve(dexp(x, rate = 1), add = TRUE, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Empirical", "Exp(1)"), col = c("lightblue", "red"), lwd = 2)


