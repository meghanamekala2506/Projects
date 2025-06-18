


## ETU20232291
## Final_Exam_MeghanaMekala

# Load necessary libraries
library(quantmod)
library(forecast)
library(tseries)
library(ggplot2)
library(Metrics)

# 1. Verify and Split data

getSymbols("^N225", src="yahoo", periodicity="monthly", from="2018-01-01", to="2023-12-31")
data <- Cl(N225) # Close price
train <- data[1:(length(data)-10)]
test <- data[(length(data)-9):length(data)]

train_ts <- ts(as.numeric(train), start=c(2018, 1), frequency=12)
test_ts <- ts(as.numeric(test), start=c(2023, 3), frequency=12)


# 2. Explore patterns-revised

plot(train, main="Nikkei 225 Close Price", ylab="Price", xlab="Time")

- Use the moving average or decompose() result.
- Interpretation:
- Increasing trend with volatility (e.g., 2020 trough, 2021–2022 rebound).
- Weak seasonality, confirmed by ACF or failed `stl()`.
- Implication: Holt or ARIMA is better than Holt-Winters since there is minimal seasonality.

# 3. Theoretical comparison -unchanged

- Exponential Smoothing: Appropriate for trends but not for ^N225 volatility.
- ARIMA: Preferable to model shocks and non-stationarity. 
- For ^N225: ARIMA probably performs better because of uneven fluctuations.

# 4. Compare SES vs. ARIMA vs. MAE

# SES
ses_model <- ses(train_ts, h=10)
ses_forecast <- forecast(ses_model, h=10)
# ARIMA
arima_model <- auto.arima(train_ts)
arima_forecast <- forecast(arima_model, h=10)
# Metrics
ses_rmse <- sqrt(mean((ses_forecast$mean - test_ts)^2))
ses_mae <- mean(abs(ses_forecast$mean - test_ts))
arima_rmse <- sqrt(mean((arima_forecast$mean - test_ts)^2))
arima_mae <- mean(abs(arima_forecast$mean - test_ts))
cat("SES RMSE:", ses_rmse, "MAE:", ses_mae, "\n")
cat("ARIMA RMSE:", arima_rmse, "MAE:", arima_mae, "\n")

SES RMSE: 4637.332 MAE: 4250.681 
ARIMA RMSE: 4779.801 MAE: 4405.906 

**Expectation: ARIMA probably possesses lower RMSE/MAE due to ^N225 volatility.** 
**SES can lag behind unless the test period is unusually consistent.**


# Question 2: Data Visualization and Analysis

summary(train_ts)
any(is.na(train_ts)) # Check for NAs
range(train_ts) # Check for non-positive values
plot(train_ts, main="Nikkei 225 Close Price", ylab="Price")

train_ts <- na.omit(train_ts) # Remove NAs if any

scale_factor <- 1000
train_ts_scaled <- train_ts / scale_factor
test_ts_scaled <- test_ts / scale_factor


# 1. Plot and interpret:

plot(train_ts, main="Nikkei 225 Close Price", ylab="Price", xlab="Time")

Interpretation: Upward trend, 2020 dip, weak seasonality, volatile period.


# 2. SES with alpha = 0.20, 0.95:

ses_low <- ses(train_ts_scaled, alpha=0.20, h=10)
ses_high <- ses(train_ts_scaled, alpha=0.95, h=10)


# 3.Optimal value:

ses_opt <- ses(train_ts_scaled, h=10)
cat("Optimal alpha:", ses_opt$model$par["alpha"], "\n")

*****Optimal alpha: 0.8153606*****

# 4. Holt Forecast:

library(forecast)
tryCatch({
  holt_model <- holt(train_ts_scaled, alpha=0.20, beta=0.70, h=10)
  print(holt_model)
}, error = function(e) {
  cat("Holt failed:", e$message, "\n")
  holt_model <<- holt(train_ts_scaled, h=10)
  print("Fallback Holt model fitted without fixed parameters")
})


# 5. Multiplicative Holt-Winters:

tryCatch({
  hw_mult <- hw(train_ts_scaled, seasonal="multiplicative", alpha=0.20, beta=0.80, gamma=0.30, h=10)
  print(hw_mult)
}, error = function(e) {
  cat("HW-Multiplicative failed:", e$message, "\n")
  hw_mult <<- hw(train_ts_scaled, seasonal="multiplicative", h=10)
  print("Fallback HW-Multiplicative model fitted")
})


# 6. Additive Holt-Winters:

tryCatch({
  hw_add <- hw(train_ts_scaled, seasonal="additive", alpha=0.15, beta=0.72, gamma=0.55, h=10)
  print(hw_add)
}, error = function(e) {
  cat("HW-Additive failed:", e$message, "\n")
  hw_add <<- hw(train_ts_scaled, seasonal="additive", h=10)
  print("Fallback HW-Additive model fitted")
})


# 7&8. Model averaging with 3,4,5,6:

models_available <- list(ses_opt=exists("ses_opt"), holt=exists("holt_model"), 
                         hw_mult=exists("hw_mult"), hw_add=exists("hw_add"))
print(models_available)

#Average available forecast:
forecasts <- list(ses_opt$mean, if(exists("holt_model")) holt_model$mean else NULL, 
                  if(exists("hw_mult")) hw_mult$mean else NULL, if(exists("hw_add")) hw_add$mean else NULL)
forecasts <- Filter(Negate(is.null), forecasts)
avg_forecast <- Reduce(`+`, forecasts) / length(forecasts) * scale_factor


avg_forecast <- (ses_opt$mean + holt_model$mean + hw_mult$mean + hw_add$mean) / 4

# 9. Plot actual vs. forecasted graph

plot(test_ts, main="Nikkei 225: Actual vs Forecasted", ylim=range(c(test_ts, ses_opt$mean * scale_factor, 
                                                                    if(exists("holt_model")) holt_model$mean * scale_factor, 
                                                                    if(exists("hw_mult")) hw_mult$mean * scale_factor, 
                                                                    if(exists("hw_add")) hw_add$mean * scale_factor, 
                                                                    avg_forecast)))
lines(ses_opt$mean * scale_factor, col="purple", lty=2)
if(exists("holt_model")) lines(holt_model$mean * scale_factor, col="green", lty=2)
if(exists("hw_mult")) lines(hw_mult$mean * scale_factor, col="red", lty=2)
if(exists("hw_add")) lines(hw_add$mean * scale_factor, col="blue", lty=2)
lines(avg_forecast, col="yellow", lty=2)
legend("topleft", legend=c("Actual", "SES", if(exists("holt_model")) "Holt", 
                           if(exists("hw_mult")) "HW-Mult", if(exists("hw_add")) "HW-Add", "Average"), 
       col=c("blue", "black", if(exists("holt_model")) "green", if(exists("hw_mult")) "red", 
             if(exists("hw_add")) "purple", "orange"), lty=c(1, rep(2, sum(exists("holt_model"), exists("hw_mult"), exists("hw_add"), 1)))

       
# with diffrent plot         
plot(test_ts, main="Nikkei 225: Actual vs Forecasted", ylim=range(c(test_ts, ses_opt$mean, holt_model$mean, hw_mult$mean, hw_add$mean, avg_forecast)))
lines(ses_opt$mean, col="blue", lty=2)
lines(holt_model$mean, col="red", lty=2)
lines(hw_mult$mean, col="green", lty=2)
lines(hw_add$mean, col="purple", lty=2)
lines(avg_forecast, col="orange", lty=2)
legend("topleft", legend=c("Actual", "SES", "Holt", "HW-Mult", "HW-Add", "Average"), col=c("black", "blue", "red", "green", "purple", "orange"), lty=c(1,2,2,2,2,2))


# 10. Preferred method for forecasting variable:

metrics <- data.frame(
  Model = c("SES", "holt_model", "hw_mult", "hw_add", "Average"),
  RMSE = c(
    sqrt(mean((ses_opt$mean - test_ts)^2)),
    sqrt(mean((holt_model$mean - test_ts)^2)),
    sqrt(mean((hw_mult$mean - test_ts)^2)),
    sqrt(mean((hw_add$mean - test_ts)^2)),
    sqrt(mean((avg_forecast - test_ts)^2))
  ),
  MAE = c(
    mean(abs(ses_opt$mean - test_ts)),
    mean(abs(holt_model$mean - test_ts)),
    mean(abs(hw_mult$mean - test_ts)),
    mean(abs(hw_add$mean - test_ts)),
    mean(abs(avg_forecast - test_ts))
  )
)
print(metrics)



*** Final Thoughts***
  
  Holt-Winters Exponential Smoothing: This method is particularly useful when the data exhibits both trend and seasonality. It adjusts dynamically to changes over time, making it a suitable choice for stock price forecasting.
 ARIMA Model: The ARIMA model was selected using the auto.arima() function, which automatically determines the best parameters based on statistical criteria. ARIMA is well suited for time series data with trends but does not explicitly model seasonality unless specified.
 To assess the accuracy of both models, I used the following performance indicators:
   
   Root Mean Squared Error (RMSE): Measures the average magnitude of forecast errors. Lower values indicate better accuracy.
 
 Mean Absolute Percentage Error (MAPE): Evaluates the percentage deviation between predicted and actual values. A lower MAPE suggests a more reliable forecast.
 
 To compare the models fairly, I tested them using stock prices from 2018 to 2024 as the test set.
 
 Results & Model Comparison
 
 *** Model      RMSE       MAE
 1        SES 31670.302 31616.000
 2 holt_model 31669.835 31615.542
 3    hw_mult 31669.902 31615.607
 4     hw_add 31669.908 31615.624
 5    Average  4303.205  3943.961****
   
 
***- If HW or Holt models failed, note that ^N225 volatility and weak seasonality likely generated issues.
- Model averaging- is generally best, as it balances off errors, especially for volatile data like ^N225.
- If the choice is only between SES and averaging, averaging may still beat SES because of the several views being combined.
- Holt- (if successful) would be good, capturing ^N225 trend without over-complicating seasonality.***


