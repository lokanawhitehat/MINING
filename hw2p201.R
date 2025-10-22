# Install required packages
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("danicamiguel/emetricsrsw")

# Load the dataset
library(emetricsrsw)
dat <- birthweight_smoking

# View dataset description
?birthweight_smoking

# Check the first few rows
head(dat)

# Load required libraries
library(ggplot2)
library(dplyr)

# Check data structure
str(dat)

# Create histograms
ggplot(dat, aes(x = birthweight, fill = as.factor(tripre0))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~ tripre0, labeller = labeller(tripre0 = c("0" = "Non-Smoker", "1" = "Smoker"))) +
  labs(title = "Distribution of Child Birthweight Based on Mother's Smoking Status",
       x = "Birthweight (grams)", y = "Count", fill = "Smoking Status") +
  theme_minimal()
