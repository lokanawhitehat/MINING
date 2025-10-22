# LOESS with span = 0.2
loess_fit1 <- loess(nox ~ dis, span = 0.2, data = Boston)

# LOESS with span = 0.5
loess_fit2 <- loess(nox ~ dis, span = 0.5, data = Boston)

# Plot both
plot(Boston$dis, Boston$nox, pch = 20, col = "gray", xlab = "dis", ylab = "nox",
     main = "Local Regression with span = 0.2 and 0.5")

lines(dis_vals, predict(loess_fit1, newdata = data.frame(dis = dis_vals)), col = "red", lwd = 2)
lines(dis_vals, predict(loess_fit2, newdata = data.frame(dis = dis_vals)), col = "blue", lwd = 2)
legend("topright", legend = c("span = 0.2", "span = 0.5"), col = c("red", "blue"), lwd = 2)
