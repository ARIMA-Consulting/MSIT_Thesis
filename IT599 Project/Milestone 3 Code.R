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
library(grid)
library(gridExtra)
library(lattice)

# Set Seed
set.seed(1337)

# Gets NASDAQ 100 Technology Sector Index (^NDXT)
tickers <- c("^NDXT")
getSymbols(tickers, src = "yahoo", from = '2006-02-21', to = '2020-04-30')

# Convert to Time Series Object and interpolate missing values
train <- na.interp(ts(NDXT$NDXT.Close))

# Create Test set for predict next month
test <- data.frame(tail(train, 20))

# April 2020 Trading days vector
AprilTradingDays2020 <- c('2020-04-01', '2020-04-02', '2020-04-03', '2020-04-06', '2020-04-07', '2020-04-08', '2020-04-09', '2020-04-13', '2020-04-14', '2020-04-15', '2020-04-16', '2020-04-17', '2020-04-20', '2020-04-21', '2020-04-22', '2020-04-23', '2020-04-24', '2020-04-27', '2020-04-28', '2020-04-29')

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
BestARIMAmodelRow$ARIMAmodelNumber

# Create Best ARIMA Forecast
BestARIMAForecast <- forecast(ARIMAmodel19, 20)

# ETS Model & Forecast
AANetsModel <- ets(train, model = "AAN")
AANetsForecast <- forecast(AANetsModel, 20)
#AANetsForecastDF <- data.frame(AANetsForecast) 

ANNetsModel <- ets(train, model = "ANN")
ANNetsForecast <- forecast(ANNetsModel, 20)
#ANNetsForecastDF <- data.frame(ANNetsForecast)

# Plot ETS Predictions
#plot(AANetsForecast)
#plot(ANNetsForecast)

# Naive Bayes Model and Forecast
naiveBayesModel <- naive(train, h=20)
naiveBayesForecast <- forecast(naiveBayesModel, 20)
#plot(naiveBayesForecast)
#naiveBayesForecastDF <- data.frame(naiveBayesForecast)

# Create Testing data Dataframe
testDF <- data.frame(test)
rownames(testDF) <- AprilTradingDays2020
names(testDF)[1] <- "Real.Data"
testDF$Real.Data <- as.numeric(testDF$Real.Data)

# Merge Model and Real Data
BestARIMAForecastDF <- data.frame(testDF, BestARIMAForecast)
AANetsForecastDF <- data.frame(testDF, AANetsForecast) 
ANNetsForecastDF <- data.frame(testDF, ANNetsForecast)
naiveBayesForecastDF <- data.frame(testDF, naiveBayesForecast)

# Statistical Tests

# Linear Models, AIC, and BIC
# ARIMA
BestARIMAlm <- lm(Real.Data ~ Point.Forecast, data = BestARIMAForecastDF)
summary(BestARIMAlm)
AIC(BestARIMAlm)
BIC(BestARIMAlm)

# AAN
AANetsLM <- lm(Real.Data ~ Point.Forecast, data = AANetsForecastDF)
summary(AANetsLM)
AIC(AANetsLM)
BIC(AANetsLM)

# ANN
ANNetsLM <- lm(Real.Data ~ Point.Forecast, data = ANNetsForecastDF)
summary(ANNetsLM)
AIC(ANNetsLM)
BIC(ANNetsLM)

# Bayes
BayesLM <- lm(Real.Data ~ Point.Forecast, data = naiveBayesForecastDF)
summary(BayesLM)
AIC(BayesLM)
BIC(BayesLM)

# ANOVA

# ARIMA
BestARIMAanova <- aov(Real.Data ~ Point.Forecast, data = BestARIMAForecastDF)
summary(BestARIMAanova)

# AAN
AANanova <- aov(Real.Data ~ Point.Forecast, data = AANetsForecastDF)
summary(AANanova)

# ANN
ANNanova <- aov(Real.Data ~ Point.Forecast, data = ANNetsForecastDF)
summary(ANNanova)

# Bayes
BayesANOVA <- aov(Real.Data ~ Point.Forecast, data = naiveBayesForecastDF)
summary(BayesANOVA)

# Plots
ggplot(data = BestARIMAForecastDF, aes(x = (row.names(BestARIMAForecastDF)), y = Real.Data, group = 1)) + geom_line(color="red") + geom_point() + geom_line(aes(y = Point.Forecast), color = "blue") + geom_line(aes(y = Lo.80), color = "black") + geom_line(aes(y = Hi.80), color = "black") + geom_line(aes(y = Lo.95), color = "gray") + geom_line(aes(y = Hi.95), color = "gray") + labs(title = "Best ARIMA Model Forecast vs Real NDXT Data", x = "Date", y = "Close Price US$")

# This is the best so far
ggplot(data = BestARIMAForecastDF, aes(x = (row.names(BestARIMAForecastDF)), y = Real.Data, group = 1)) +
  geom_line(color="red") + geom_point() + 
  geom_line(aes(y = Point.Forecast), color = "blue") + 
  geom_line(aes(y = Lo.80), color = "black") + 
  geom_line(aes(y = Hi.80), color = "black") + 
  geom_line(aes(y = Lo.95), color = "gray") + 
  geom_line(aes(y = Hi.95), color = "gray") + 
  labs(title = "Best ARIMA Model Forecast vs Real NDXT Data", x = "Date", y = "Close Price US$") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

ggplot(data = BestARIMAForecastDF, aes(x = (row.names(BestARIMAForecastDF)), group = 1)) +
  geom_line(aes(y = Real.Data, color="red"), group = 1) +
  geom_line(aes(y = Point.Forecast, color = "blue"), group = 1) + 
  geom_line(aes(y = Lo.80, color = c("#000000")), group = 1) + 
  geom_line(aes(y = Hi.80, color = c("#00000d")), group = 1) + 
  geom_line(aes(y = Lo.95, color = c("#8c8c8c")), group = 1) + 
  geom_line(aes(y = Hi.95, color = c("#808080")), group = 1) + 
  labs(title = "Best ARIMA Model Forecast vs Real NDXT Data", x = "Date", y = "Close Price US$") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

#Closest I got
ggplot(data = BestARIMAForecastDF) + 
  aes(x = (row.names(BestARIMAForecastDF))) +
  geom_line(aes(y = Real.Data, color = "Real Data", group = 1), color = "red") +
  geom_line(aes(y = Point.Forecast, color = "Forecast", group = 1)) + 
  geom_line(aes(y = Lo.80, color = "80% Confidence"), group = 1) + 
  geom_line(aes(y = Hi.80, color = "80% Confidence"), group = 1) + 
  geom_line(aes(y = Lo.95, color = "95% Confidence"), group = 1) + 
  geom_line(aes(y = Hi.95, color = "95% Confidence"), group = 1) + 
  labs(title = "Best ARIMA Model Forecast vs Real NDXT Data", x = "Date", y = "Close Price US$") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

# Test
ggplot(data = BestARIMAForecastDF) + 
  aes(x = (row.names(BestARIMAForecastDF)), group = 1) +
  geom_line(aes(y = Real.Data), labs(title = "test")) +
  geom_line(aes(y = Point.Forecast)) + 
  geom_line(aes(y = Lo.80)) + 
  geom_line(aes(y = Hi.80)) + 
  geom_line(aes(y = Lo.95)) + 
  geom_line(aes(y = Hi.95)) + 
  labs(title = "Best ARIMA Model Forecast vs Real NDXT Data", x = "Date", y = "Close Price US$") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))


