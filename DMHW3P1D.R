library(splines)

fit_spline4 <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(fit_spline4)

# Plot spline fit
plot(Boston$dis, Boston$nox, pch = 20, col = "gray", xlab = "dis", ylab = "nox")
lines(dis_vals, predict(fit_spline4, newdata = data.frame(dis = dis_vals)), col = "red", lwd = 2)
