cv_spline <- sapply(3:10, function(df) {
  fit <- glm(nox ~ bs(dis, df = df), data = Boston)
  cv.glm(Boston, fit, K = 10)$delta[1]
})

# Plot
plot(3:10, cv_spline, type = "b", xlab = "Degrees of Freedom", ylab = "CV Error")
