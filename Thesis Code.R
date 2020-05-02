# Load Libraries
library(quantmod)
library(tseries)
library(xts)
library(forecast)

# Gets NASDAQ 100 Technology Sector Index (^NDXT)
tickers <- c("^NDXT")
getSymbols(tickers, src = "yahoo", from = '2012-01-01', to = '2020-05-01')

# Head of NDXT
head(NDXT)

# Summary of Data
summary(NDXT)

# Plot Close Price
plot(NDXT$NDXT.Close)

# Create Univariate Time Series
NDXTclose <- ts(NDXT$NDXT.Close)

# Auto ARIMA Test
# Create fit model
fit <- auto.arima(NDXTclose)

# see results
fit

# Create forecast
forecastAutoARIMA <- forecast(fit, 20)

# Quick plot of Auto ARIMA model forecast
plot(forecastAutoARIMA)

# Check Accuracy of Auto ARIMA model
accuracy(fit)

# Check Normality Assumption
qqnorm(fit$residuals)
qqline(fit$residuals)

# Ljung-Box Test
Box.test(fit$residuals, type = "Ljung-Box")

# Seasonal Decomposition









