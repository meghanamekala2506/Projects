## ETU20232291
## Meghana Mekala

# Install required packages if not already installed
if (!require(quantmod)) install.packages("quantmod")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

# Load necessary libraries
library(quantmod)
library(forecast)
library(tseries)

# Download stock price data for Apple (AAPL) from Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2019-01-01", to = "2024-12-31", periodicity = "monthly")

# Extract adjusted close price for monthly stock data
apple_stock <- Cl(AAPL)

# Plot the data to understand the trends
plot(apple_stock, main = "Apple Stock Price (Jan 2019 - Dec 2024)", ylab = "Stock Price", xlab = "Date")

# Convert the data to a time series object monthly frequency
apple_ts <- ts(apple_stock, frequency = 12, start = c(2019, 1))

# Check for missing values
sum(is.na(apple_ts))

# If there are missing values, you can use interpolation or forward filling to handle them
apple_ts <- na.approx(apple_ts)

# Fit Holt-Winters Exponential Smoothing Model
holt_winters_model <- HoltWinters(apple_ts)

# Plot the Holt-Winters model
plot(holt_winters_model)

# Forecast the next 12 months
holt_winters_forecast <- forecast(holt_winters_model, h = 12)

# Plot the forecast
plot(holt_winters_forecast, main = "Holt-Winters Forecast for Apple Stock Price", ylab = "Stock Price", xlab = "Date")

# Fit the ARIMA model
arima_model <- auto.arima(apple_ts)

# Summary of the ARIMA model
summary(arima_model)

# Forecast the next 12 months
arima_forecast <- forecast(arima_model, h = 12)

# Plot the ARIMA forecast
plot(arima_forecast, main = "ARIMA Forecast for Apple Stock Price", ylab = "Stock Price", xlab = "Date")


# Split the data into training and test sets
train_data <- window(apple_ts, end = c(2023, 12))  # Training data until December 2023
test_data <- window(apple_ts, start = c(2024, 1))  # Test data from January 2024 onward

# Fit the Holt-Winters model on the training data
holt_winters_model <- HoltWinters(train_data)

# Forecast using Holt-Winters on the test set length
holt_winters_forecast_test <- forecast(holt_winters_model, h = length(test_data))

# Fit the ARIMA model on the training data
arima_model <- auto.arima(train_data)

# Forecast using ARIMA on the test set length
arima_forecast_test <- forecast(arima_model, h = length(test_data))

# Calculate accuracy metrics for Holt-Winters model
holt_winters_accuracy <- accuracy(holt_winters_forecast_test, test_data)
holt_winters_accuracy

# Calculate accuracy metrics for ARIMA model
arima_accuracy <- accuracy(arima_forecast_test, test_data)
arima_accuracy

# Compare RMSE, MAE, MAPE
holt_winters_rmse <- holt_winters_accuracy["Test set", "RMSE"]
arima_rmse <- arima_accuracy["Test set", "RMSE"]

holt_winters_mape <- holt_winters_accuracy["Test set", "MAPE"]
arima_mape <- arima_accuracy["Test set", "MAPE"]

# Print the results for comparison
cat("Holt-Winters RMSE: ", holt_winters_rmse, "\n")
cat("ARIMA RMSE: ", arima_rmse, "\n")
cat("Holt-Winters MAPE: ", holt_winters_mape, "\n")
cat("ARIMA MAPE: ", arima_mape, "\n")


# Load required library
library(ggplot2)

# Create a data frame with the error metrics
error_metrics <- data.frame(
  Model = c("Holt-Winters", "ARIMA"),
  RMSE = c(holt_winters_rmse, arima_rmse),
  MAPE = c(holt_winters_mape, arima_mape)
)

# Reshape data for ggplot
library(reshape2)
error_metrics_melted <- melt(error_metrics, id.vars = "Model", variable.name = "Metric", value.name = "Value")

# Create the bar plot
ggplot(error_metrics_melted, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Forecasting Model Errors", x = "Model", y = "Error Value") +
  theme_minimal() +
  scale_fill_manual(values = c("RMSE" = "steelblue", "MAPE" = "darkorange"))






## Forecasting Apple Stock Prices Using Exponential Smoothing and ARIMA Models

Methodology

For this analysis, I explored two different time series forecasting models to predict Apple’s stock prices: Holt-Winters Exponential Smoothing and ARIMA (Auto-Regressive Integrated Moving Average). The dataset used contains monthly stock prices from January 2019 to December 2024.

Data Collection & Preparation

The historical stock price data was sourced from Yahoo Finance, focusing on the adjusted closing prices for each month. Once the data was collected, I transformed it into a time series format to ensure proper analysis. Any missing values were handled using interpolation techniques to maintain data continuity.

Model Selection & Forecasting

Holt-Winters Exponential Smoothing: This method is particularly useful when the data exhibits both trend and seasonality. It adjusts dynamically to changes over time, making it a suitable choice for stock price forecasting.

ARIMA Model: The ARIMA model was selected using the auto.arima() function, which automatically determines the best parameters based on statistical criteria. ARIMA is well suited for time series data with trends but does not explicitly model seasonality unless specified.

Evaluation Metrics

To assess the accuracy of both models, I used the following performance indicators:
  
  Root Mean Squared Error (RMSE): Measures the average magnitude of forecast errors. Lower values indicate better accuracy.

Mean Absolute Percentage Error (MAPE): Evaluates the percentage deviation between predicted and actual values. A lower MAPE suggests a more reliable forecast.

To compare the models fairly, I tested them using stock prices from January 2024 to December 2024 as the test set.

Results & Model Comparison

Holt-Winters Model:
  
  RMSE: 13.03

MAPE: 5.56%

The Holt-Winters model effectively captured both the trend and seasonality present in the stock prices, resulting in relatively accurate forecasts.

ARIMA Model:
  
  RMSE: 18.77

MAPE: 8.34%

While the ARIMA model was able to capture the underlying trend, its performance was weaker compared to Holt-Winters. This could be because ARIMA does not explicitly account for seasonal patterns, which might have affected its forecasting accuracy.

Interpretation & Conclusion

The evaluation metrics clearly indicate that Holt-Winters outperformed ARIMA in this case. With a lower RMSE and MAPE, Holt-Winters provided more accurate and reliable predictions. The presence of seasonality in Apple’s stock prices likely gave Holt-Winters an advantage over ARIMA, which focuses more on trend-based patterns.

In summary, based on the results, Holt-Winters Exponential Smoothing is the better choice for forecasting Apple’s stock prices in this scenario. Its ability to incorporate both trend and seasonality made it more effective in predicting future prices compared to ARIMA.