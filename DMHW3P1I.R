cv_ns <- sapply(3:10, function(df) {
  fit <- glm(nox ~ ns(dis, df = df), data = Boston)
  cv.glm(Boston, fit, K = 10)$delta[1]
})

# Plot
plot(3:10, cv_ns, type = "b", xlab = "Degrees of Freedom", ylab = "CV Error",
     main = "CV for Natural Cubic Spline")
