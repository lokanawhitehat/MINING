library(MASS)
data("Boston")

# Fit cubic polynomial regression
fit_poly3 <- lm(nox ~ poly(dis, 3), data = Boston)
summary(fit_poly3)

# Plot data and polynomial fit
plot(Boston$dis, Boston$nox, main = "Cubic Polynomial Fit", xlab = "dis", ylab = "nox", pch = 20, col = "gray")
dis_vals <- seq(min(Boston$dis), max(Boston$dis), length.out = 100)
preds <- predict(fit_poly3, newdata = data.frame(dis = dis_vals))
lines(dis_vals, preds, col = "blue", lwd = 2)
