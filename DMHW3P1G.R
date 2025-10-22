library(splines)

# Fit natural cubic spline with df = 4
fit_ns4 <- lm(nox ~ ns(dis, df = 4), data = Boston)
summary(fit_ns4)

# Plot
plot(Boston$dis, Boston$nox, pch = 20, col = "gray", xlab = "dis", ylab = "nox",
     main = "Natural Cubic Spline (df = 4)")
lines(dis_vals, predict(fit_ns4, newdata = data.frame(dis = dis_vals)), col = "darkgreen", lwd = 2)
