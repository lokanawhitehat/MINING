# Set seed for reproducibility
set.seed(123)  

# Sample size
n <- 1000  

# Case I: X ~ Uniform(-1,1), Y = 2X^2 + e, where e ~ N(0, 0.5)
X1 <- runif(n, -1, 1)  
e1 <- rnorm(n, mean = 0, sd = 0.5)  
Y1 <- 2 * X1^2 + e1  

# Fit a linear model
fit1 <- lm(Y1 ~ X1)  
summary(fit1)  

# Case II: X ~ N(0.5, 0.5), Y = 2X^2 + e, where e ~ N(0, 0.5)
X2 <- rnorm(n, mean = 0.5, sd = 0.5)  
e2 <- rnorm(n, mean = 0, sd = 0.5)  
Y2 <- 2 * X2^2 + e2  

# Fit a linear model
fit2 <- lm(Y2 ~ X2)  
summary(fit2)  

# Compare estimated coefficients
cat("Estimated Coefficients for Uniform X:\n")  
print(coef(fit1))  
cat("Estimated Coefficients for Normal X:\n")  
print(coef(fit2))  

# Visualization
par(mfrow = c(1, 2))  
plot(X1, Y1, main = "Uniform X", xlab = "X", ylab = "Y")  
abline(fit1, col = "red", lwd = 2)  
plot(X2, Y2, main = "Normal X", xlab = "X", ylab = "Y")  
abline(fit2, col = "blue", lwd = 2)  
