# Load necessary libraries
install.packages("emetricsrsw")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("pROC")
install.packages("mgcv")

library(emetricsrsw)
library(caret)
library(ggplot2)
library(dplyr)
library(pROC)
library(mgcv)

# Load dataset
dat <- as.data.frame(birthweight_smoking)

# View column names
colnames(dat)

# Fit logistic regression model
logit_model <- glm(smoker ~ . - tripre0 - birthweight, data = dat, family = binomial)

# Summary of logistic regression model
summary(logit_model)

# Convert log-odds to odds ratios
exp(coef(logit_model))

# Compute predicted probabilities
dat$predicted_probs <- predict(logit_model, type = "response")

# Train-test split
set.seed(123)
split <- createDataPartition(dat$smoker, p = 0.7, list = FALSE)
train_data <- dat[split, ]
test_data <- dat[-split, ]

# Fit logistic model on training data
logit_model <- glm(smoker ~ . - tripre0 - birthweight, data = train_data, family = binomial)

# Predict on test data
test_data$predicted_probs <- predict(logit_model, newdata = test_data, type = "response")

# Divide predictions into 10 groups (deciles)
test_data <- test_data %>%
  mutate(decile = ntile(predicted_probs, 10))

# Compute calibration statistics
calibration_data <- test_data %>%
  group_by(decile) %>%
  summarise(
    avg_pred_prob = mean(predicted_probs),
    avg_actual = mean(smoker),
    n = n()
  ) %>%
  mutate(std_error = sqrt(avg_actual * (1 - avg_actual) / (n - 1)))

# Plot calibration curve
ggplot(calibration_data, aes(x = avg_pred_prob, y = avg_actual)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_actual - std_error, ymax = avg_actual + std_error), width = 0.02) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Calibration Plot", x = "Average Predicted Probability", y = "Observed Smoking Rate") +
  theme_minimal()

# Compute ROC curve
roc_obj <- roc(test_data$smoker, test_data$predicted_probs)

# Print AUC value
auc_value <- roc_obj$auc
print(paste("AUC:", auc_value))

# Find the best threshold
best_cutoff <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
print(paste("Best Cutoff:", best_cutoff))

# Convert predicted probabilities into binary classifications
test_data$predicted_class <- ifelse(test_data$predicted_probs >= as.numeric(best_cutoff), 1, 0)

# Create confusion matrix
conf_matrix <- table(Predicted = test_data$predicted_class, Actual = test_data$smoker)
print(conf_matrix)

# Compute Sensitivity and Specificity
sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
specificity <- conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[1, 2])

# Print results
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Check for missing values
sum(is.na(test_data$smoker))
sum(is.na(test_data$predicted_probs))

# Convert 'smoker' and 'predicted_probs' to numeric
test_data$smoker <- as.numeric(test_data$smoker)
test_data$predicted_probs <- as.numeric(test_data$predicted_probs)

# Remove NA values
test_data <- na.omit(test_data)

# Compute the ROC curve again
roc_obj <- roc(test_data$smoker, test_data$predicted_probs)

# Print AUC
auc_value <- roc_obj$auc
print(paste("AUC:", auc_value))

# Fit a linear regression model
lm_model <- lm(birthweight ~ . - tripre0 - smoker, data = dat)

# View model summary
summary(lm_model)

# Fit a Generalized Additive Model (GAM)
gam_model <- mgcv::gam(
  birthweight ~ s(nprevist, k = 15) + alcohol + tripre1 + tripre2 + 
    tripre3 + unmarried + s(educ, k = 15) + s(age, k = 15) + drinks,
  data = dat
)

# View GAM model summary
summary(gam_model)

# Plot smooth terms to visualize nonlinear effects
plot(gam_model, pages = 1)

# Step 1: Fit models to get residuals
lm_Y <- lm(birthweight ~ ., data = dat)  # Predict birthweight using all covariates
lm_A <- lm(smoker ~ ., data = dat)  # Predict smoking status using all covariates

# Step 2: Extract residuals
residual_Y <- residuals(lm_Y)
residual_A <- residuals(lm_A)

# Step 3: Fit regression of residual_Y on residual_A (no intercept)
beta_hat <- coef(lm(residual_Y ~ residual_A - 1))  # "-1" removes intercept

# Print the estimated β
print(beta_hat)
