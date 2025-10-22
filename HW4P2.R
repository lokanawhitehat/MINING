# Load required libraries
install.packages("faraway")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("MASS")

library(faraway)
library(ggplot2)
library(dplyr)
library(MASS)

# Load the dataset
data(dvisits)

# View dataset structure
str(dvisits)

# View first few rows
head(dvisits)

# Scatterplot: Doctor Visits vs. Age
ggplot(dvisits, aes(x = age, y = doctorco)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Doctor Visits vs. Age", x = "Age", y = "Number of Doctor Visits") +
  theme_minimal()

# Scatterplot: Doctor Visits vs. Number of Illnesses
ggplot(dvisits, aes(x = illness, y = doctorco)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Doctor Visits vs. Number of Illnesses", 
       x = "Number of Illnesses", y = "Number of Doctor Visits") +
  theme_minimal()

# Boxplot: Doctor Visits by Illness Count
ggplot(dvisits, aes(x = as.factor(illness), y = doctorco)) +
  geom_boxplot() +
  labs(title = "Doctor Visits Distribution by Number of Illnesses", 
       x = "Number of Illnesses", y = "Doctor Visits") +
  theme_minimal()

# Create categorical variable for chronic condition status
dvisits <- dvisits %>%
  mutate(chronic_status = case_when(
    chcond1 == 0 & chcond2 == 0 ~ "None",
    chcond1 + chcond2 == 1 ~ "One",
    chcond1 == 1 & chcond2 == 1 ~ "Multiple"
  ))

# Convert to factor
dvisits$chronic_status <- factor(dvisits$chronic_status, levels = c("None", "One", "Multiple"))

# Check new variable distribution
table(dvisits$chronic_status)

# Boxplot: Doctor Visits by Chronic Condition Status
ggplot(dvisits, aes(x = chronic_status, y = doctorco, fill = chronic_status)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Doctor Visits by Chronic Condition Status",
       x = "Chronic Condition Status",
       y = "Number of Doctor Visits") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "orange", "red"))

# Fit Poisson regression model
poisson_model <- glm(doctorco ~ sex + age + agesq + income + levyplus +
                       freepoor + freerepa + illness + actdays + hscore + chronic_status,
                     family = poisson, data = dvisits)

# Summary of Poisson model
summary(poisson_model)

# Extract residual deviance and degrees of freedom
residual_deviance <- poisson_model$deviance
degrees_freedom <- poisson_model$df.residual
p_value <- 1 - pchisq(residual_deviance, df = degrees_freedom)

# Print results
cat("Residual Deviance:", residual_deviance, "\n")
cat("Degrees of Freedom:", degrees_freedom, "\n")
cat("P-value:", p_value, "\n")

# Fit Negative Binomial model
negbin_model <- glm.nb(doctorco ~ sex + age + agesq + income + levyplus +
                         freepoor + freerepa + illness + actdays + hscore + chronic_status,
                       data = dvisits)

# Summary of Negative Binomial Model
summary(negbin_model)

# Extract deviance residuals and fitted values
deviance_residuals <- residuals(poisson_model, type = "deviance")
fitted_values <- fitted(poisson_model)

# Deviance Residuals vs. Fitted Values
ggplot(data = data.frame(fitted_values, deviance_residuals), aes(x = fitted_values, y = deviance_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Deviance Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Deviance Residuals") +
  theme_minimal()

# Pearson Residuals vs. Fitted Values
pearson_residuals <- residuals(poisson_model, type = "pearson")

ggplot(data = data.frame(fitted_values, pearson_residuals), aes(x = fitted_values, y = pearson_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Pearson Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal()

# Q-Q plot of deviance residuals
qqnorm(deviance_residuals)
qqline(deviance_residuals, col = "red")

# Compute residual deviance and degrees of freedom
residual_deviance <- poisson_model$deviance
degrees_freedom <- poisson_model$df.residual
p_value <- 1 - pchisq(residual_deviance, df = degrees_freedom)

# Print results
cat("Residual Deviance:", residual_deviance, "\n")
cat("Degrees of Freedom:", degrees_freedom, "\n")
cat("P-value:", p_value, "\n")

# Extract predicted values from Negative Binomial model
exp(coef(negbin_model))

# Extract the last person's data
last_person <- tail(dvisits, 1)

# Compute predicted lambda (expected visit count)
lambda <- predict(negbin_model, newdata = last_person, type = "response")

# Print expected visit count
cat("Expected Doctor Visits (Lambda):", lambda, "\n")

# Extract dispersion parameter (Theta)
theta <- negbin_model$theta

# Compute probability distribution for 0 to 10 doctor visits
visit_counts <- 0:10
visit_probs <- dnbinom(visit_counts, size = theta, mu = lambda)

# Create a dataframe for visualization
prob_df <- data.frame(Visits = visit_counts, Probability = visit_probs)

# Print probability table
print(prob_df)

# Plot predicted probability distribution
ggplot(prob_df, aes(x = Visits, y = Probability)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Predicted Probability Distribution of Doctor Visits",
       x = "Number of Visits",
       y = "Probability") +
  theme_minimal()
