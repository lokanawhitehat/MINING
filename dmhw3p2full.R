# Load necessary libraries
library(ISLR)       # For College dataset
library(caret)      # For data splitting
library(leaps)      # For stepwise selection
library(mgcv)       # For GAM models
library(ggplot2)    # For plotting
library(patchwork)  # For combining plots

# Load the College dataset
data(College)

# Convert Private to factor if not already
College$Private <- as.factor(College$Private)

## Part (a): Split data and perform forward stepwise selection

# Set seed for reproducibility
set.seed(123)

# Create training/test split (70/30)
train_index <- createDataPartition(College$Outstate, p = 0.7, list = FALSE)
train_data <- College[train_index, ]
test_data <- College[-train_index, ]

# Forward stepwise selection
forward_fit <- regsubsets(Outstate ~ ., data = train_data, 
                          nvmax = 15, method = "forward")

# Find best model using BIC
forward_summary <- summary(forward_fit)
best_model_size <- which.min(forward_summary$bic)

# Get selected variables (convert PrivateYes back to Private)
selected_vars <- names(coef(forward_fit, best_model_size))[-1]
selected_vars <- gsub("PrivateYes", "Private", selected_vars)

# Print selected variables
cat("Selected variables from forward selection:\n")
print(selected_vars)

## Part (b): Fit GAM on selected variables

# Create formula for GAM with smooth terms for continuous variables
# Handle Private separately as it's a factor
continuous_vars <- setdiff(selected_vars, "Private")
smooth_terms <- if(length(continuous_vars) > 0) {
  paste("s(", continuous_vars, ")", collapse = " + ")
} else { "1" }

gam_formula <- as.formula(
  paste("Outstate ~ Private +", smooth_terms)
)

# Fit GAM model
gam_fit <- gam(gam_formula, data = train_data, method = "REML")

# Summary of GAM
summary(gam_fit)

# Plot smooth terms
gam_plots <- plot(gam_fit, pages = 1, residuals = TRUE, pch = 1, cex = 1)
print(gam_plots)

## Part (c): Evaluate model on test set

# Predictions on test set
test_pred <- predict(gam_fit, newdata = test_data)

# Calculate RMSE and R-squared
test_rmse <- sqrt(mean((test_pred - test_data$Outstate)^2))
test_rsq <- cor(test_pred, test_data$Outstate)^2

cat("\nTest set performance:\n")
cat("RMSE:", round(test_rmse, 2), "\n")
cat("R-squared:", round(test_rsq, 4), "\n")

## Part (d): Identify non-linear relationships

# Extract p-values for smooth terms to check non-linearity
gam_summary <- summary(gam_fit)
smooth_pvals <- gam_summary$s.table[, "p-value"]

cat("\nNon-linear relationship analysis:\n")
for (i in 1:length(smooth_pvals)) {
  var_name <- names(smooth_pvals)[i]
  pval <- smooth_pvals[i]
  if (pval < 0.05) {
    cat(var_name, ": Strong evidence of non-linear relationship (p =", round(pval, 4), ")\n")
  } else {
    cat(var_name, ": No strong evidence of non-linear relationship (p =", round(pval, 4), ")\n")
  }
}

# Create enhanced plots for non-linear relationships
plot_list <- list()
for (var in continuous_vars) {
  p <- ggplot(train_data, aes_string(x = var, y = "Outstate")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "gam", formula = y ~ s(x)) +
    ggtitle(paste("Outstate vs", var)) +
    theme_minimal()
  plot_list[[var]] <- p
}

# Combine plots
if(length(plot_list) > 0) {
  combined_plots <- wrap_plots(plot_list, ncol = 2)
  print(combined_plots)
}