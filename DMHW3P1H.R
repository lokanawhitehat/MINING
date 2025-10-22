rss_ns <- numeric(10)

for (df in 3:10) {
  fit <- lm(nox ~ ns(dis, df = df), data = Boston)
  rss_ns[df] <- sum(residuals(fit)^2)
}

# View RSS for df = 3 to 10
rss_ns[3:10]
