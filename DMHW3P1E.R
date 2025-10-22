rss_spline <- numeric(10)

for (df in 3:10) {
  fit <- lm(nox ~ bs(dis, df = df), data = Boston)
  rss_spline[df] <- sum(residuals(fit)^2)
}

# Report RSS
rss_spline[3:10]
