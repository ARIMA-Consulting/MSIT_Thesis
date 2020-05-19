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
getSymbols(tickers, src = "yahoo", from = '2006-02-21', to = '2020-04-30')

# Convert to Time Series Object and interpolate missing values
train <- na.interp(ts(NDXT$NDXT.Close))

# Create Test set for predict next month
test <- data.frame(tail(train, 21))

# April 2020 Trading days vector
AprilTradingDays2020 <- c('2020-04-01', '2020-04-02', '2020-04-03', '2020-04-06', '2020-04-07', '2020-04-08', '2020-04-09', '2020-04-13', '2020-04-14', '2020-04-15', '2020-04-16', '2020-04-17', '2020-04-20', '2020-04-21', '2020-04-22', '2020-04-23', '2020-04-24', '2020-04-27', '2020-04-28', '2020-04-29', '2020-04-30')

# Make Cartesian Product of p, d, and q values
p <- c(0,1,2,3)
d <- c(0,1,2,3)
q <- c(0,1,2,3)
cartesianProduct = expand.grid(p = p, d = d, q = q)

# Cleans cartesian product when d = 0 and p does not equal 0 because this is an invalid condition
cartesianProduct <- cartesianProduct[!(cartesianProduct$d == 0 & cartesianProduct$p != 0),]

# Fix Index column in cartesian product to be in order
rownames(cartesianProduct) <- c(1:nrow(cartesianProduct))

# Create Accuracy DF to hold accuracy info for the ARIMA Models
AccuracyDF <- data.frame(matrix(ncol = 8, nrow = nrow(cartesianProduct)))
vecOfColNames <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "ARIMAmodelNumber")
colnames(AccuracyDF) <- vecOfColNames

# Creates ARIMA models and fills the Accuracy DF with all the accuracy stats. Conveniently, the accuracy of the Models and forecasts is the same.
for(i in rownames(cartesianProduct)){
  AccuracyDF[i,] <- c(accuracy(assign(paste0("ARIMAmodel", i), try(arima(train, unlist(cartesianProduct[i,])), silent = TRUE))),i)
}

# Gets row of Model with lowest MAPE
BestARIMAmodelRow <- AccuracyDF[which.min(AccuracyDF$MAPE),]
