library(boot)
cv_error <- sapply(1:10, function(d) {
  glm_fit <- glm(nox ~ poly(dis, d), data = Boston)
  cv.glm(Boston, glm_fit, K = 10)$delta[1]
})

# Plot CV error vs degree
plot(1:10, cv_error, type = "b", xlab = "Degree", ylab = "CV Error")
