# IT599 Project Code
# Tyler Harris
# Purdue University Global

# Load Libraries
library(quantmod)
library(tseries)
library(xts)
library(forecast)
library(tibble)
library(stinepack)
library(ggplot2)

# Set Seed
set.seed(1337)

# Gets NASDAQ 100 Technology Sector Index (^NDXT)
tickers <- c("^NDXT")
getSymbols(tickers, src = "yahoo", from = '2006-02-21', to = '2020-03-31')

# Head of NDXT
head(NDXT)

# Summary of Data
summary(NDXT)

# Plot Close Price
plot(NDXT$NDXT.Close)

# Create Univariate Time Series
NDXTclose <- na.interp(ts(NDXT$NDXT.Close))
NDXTclose <- na.interp(NDXTclose)

# Check Normality Assumption
qqnorm(fit$residuals)
qqline(fit$residuals)

# Auto ARIMA Test
# Create fit model
fit <- auto.arima(NDXTclose)

# see results
fit

# Create forecast
forecastAutoARIMA <- forecast(fit, 30)

# Quick plot of Auto ARIMA model forecast
plot(forecastAutoARIMA)

# Check Accuracy of Auto ARIMA model
accuracy(fit)

# Ljung-Box Test
Box.test(fit$residuals, type = "Ljung-Box")

# Different ARIMA
ARIMA212 <- arima(NDXTclose, order = c(2,1,2))
ARIMA212
ARIMA212forecast <- forecast(ARIMA212, 30)
plot(ARIMA212forecast)

# ETS Model Predictions
demofit <- ets(NDXTclose, model = "AAN")
demofitForecast <- forecast(demofit, 30)
demofit

demofit2 <- ets(NDXTclose, model = "ANN")
demofit2Forecast <- forecast(demofit2, 30)
demofit2

# Plot ETS Predictions
plot(demofitForecast)
plot(demofit2Forecast)

# Naive Bayes Test
naiveTest <- naive(NDXTclose, h=30)
naiveForecast <- forecast(naiveTest, 30)
plot(naiveForecast)
naiveForecast