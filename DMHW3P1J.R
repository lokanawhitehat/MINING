set.seed(1)
jittered_dis <- jitter(Boston$dis, amount = 1e-3)  # was 1e-5 before

cv_smooth_jittered <- smooth.spline(jittered_dis, Boston$nox, cv = TRUE)
cv_smooth_jittered$df

# Plot
plot(Boston$dis, Boston$nox, pch = 20, col = "gray",
     main = "Smoothing Spline with Jittered dis (df by CV)", xlab = "dis", ylab = "nox")
lines(smooth.spline(jittered_dis, Boston$nox, df = cv_smooth_jittered$df), col = "purple", lwd = 2)

