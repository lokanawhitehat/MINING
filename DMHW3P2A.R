library(ISLR)
library(leaps)

# Load the College data
data(College)

# Split into train/test
set.seed(42)
train_idx <- sample(1:nrow(College), nrow(College) * 0.7)
train <- College[train_idx, ]
test <- College[-train_idx, ]

# Perform forward stepwise selection
regfit_fwd <- regsubsets(Outstate ~ ., data = train, method = "forward")
summary_fwd <- summary(regfit_fwd)

# View model performance (e.g., adjusted R² or Cp)
which.max(summary_fwd$adjr2)
best_vars <- names(coef(regfit_fwd, which.max(summary_fwd$adjr2)))[-1]  # Selected variables
best_vars
