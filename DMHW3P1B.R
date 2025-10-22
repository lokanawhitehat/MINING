rss <- numeric(10)
plot(Boston$dis, Boston$nox, col = "lightgray", pch = 20, xlab = "dis", ylab = "nox")

for (d in 1:10) {
  fit <- lm(nox ~ poly(dis, d), data = Boston)
  rss[d] <- sum(residuals(fit)^2)
  
  preds <- predict(fit, newdata = data.frame(dis = dis_vals))
  lines(dis_vals, preds, col = rainbow(10)[d])
}

# Print RSS
rss
