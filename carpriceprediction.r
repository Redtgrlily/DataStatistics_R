# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Set working directory and load datasets
# setwd("dir")  # Adjust as needed
set.seed(123)

# Create a mock training dataset
training_data <- data.frame(
  Price = round(runif(1000, 5000, 40000)),
  Mileage = round(runif(1000, 5000, 200000)),
  Age = sample(1:15, 1000, replace = TRUE),
  EngineSize = round(runif(1000, 1.0, 5.0), 1),
  BrandValue = sample(1:5, 1000, replace = TRUE)
)

# Create a mock testing dataset
testing_data <- data.frame(
  Price = round(runif(460, 5000, 40000)),
  Mileage = round(runif(460, 5000, 200000)),
  Age = sample(1:15, 460, replace = TRUE),
  EngineSize = round(runif(460, 1.0, 5.0), 1),
  BrandValue = sample(1:5, 460, replace = TRUE)
)

# Check structure of the dataset
str(testing_data)

# Summary statistics for car price
price_stats <- testing_data %>%
  summarise(
    Minimum = min(Price, na.rm = TRUE),
    Maximum = max(Price, na.rm = TRUE),
    Mean = mean(Price, na.rm = TRUE),
    Median = median(Price, na.rm = TRUE),
    SD = sd(Price, na.rm = TRUE)
  )

print("Summary Statistics for Car Price (Testing Data):")
print(price_stats)

# Histogram for car price distribution
hist_car_prices <- ggplot(testing_data, aes(x = Price)) +
  geom_histogram(binwidth = 1500, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Car Prices (Testing Data)",
       x = "Car Price ($)", y = "Count") +
  theme_minimal()
print(hist_car_prices)

# Combine training and testing for visualization
combined_data <- bind_rows(training_data, testing_data)

hist_combined <- ggplot(combined_data, aes(x = Price)) +
  geom_histogram(binwidth = 1500, fill = "orange", color = "black", alpha = 0.6) +
  labs(title = "Combined Distribution of Car Prices",
       x = "Car Price ($)", y = "Count") +
  theme_minimal()
print(hist_combined)

# Build linear regression model
training_data_clean <- training_data %>% drop_na()
lm_model <- lm(Price ~ ., data = training_data_clean)

# Model summary
print(summary(lm_model))

# Predict on test data (first 20 rows with complete cases)
testing_data_clean <- testing_data %>% drop_na()
testing_subset <- head(testing_data_clean, 20)

predicted <- predict(lm_model, newdata = testing_subset)

# Compare actual vs predicted
results <- data.frame(
  Actual = testing_subset$Price,
  Predicted = predicted,
  Error = testing_subset$Price - predicted,
  Percent_Error = 100 * abs(testing_subset$Price - predicted) / testing_subset$Price
)

print("Prediction Results:")
print(results)

# Compute RMSE and MAPE
rmse <- sqrt(mean(results$Error^2))
mape <- mean(results$Percent_Error)

cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")

# Scatter plot for actual vs predicted
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Car Prices",
       x = "Actual Price ($)", y = "Predicted Price ($)") +
  theme_light()
