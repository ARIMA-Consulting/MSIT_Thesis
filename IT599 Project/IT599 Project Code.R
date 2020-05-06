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
tickers <- c("^NDXT", "AAPL")
getSymbols(tickers, src = "yahoo", from = '2006-02-21', to = '2020-04-30')

# Convert to Time Series Object and interpolate missing values
train <- na.interp(ts(NDXT$NDXT.Close))

test <- data.frame(tail(train, 20))

# Make Cartesian Product of p, d, and q values
p <- c(0,1,2,3)
d <- c(0,1,2,3)
q <- c(0,1,2,3)
cartesianProduct = expand.grid(p = p, d = d, q = q)

# OMG THIS WORKS

for(i in rownames(cartesianProduct)){
  assign(paste0("ARIMAmodel", i), try(arima(train, unlist(cartesianProduct[i,])), silent = TRUE))
  #assign(paste0("ARIMAforecast", i), try(forecast(NDXTclose, 30), silent = TRUE))
}

# Create Accuracy DF
AccuracyDF <- data.frame(matrix(ncol = 8, nrow = 52))
vecOfColNames <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1", "ARIMAmodelNumber")
colnames(AccuracyDF) <- vecOfColNames

# Vector of ARIMA models with no Errors
ARIMAmodelsThatWork <- c(1,5,6,7,8,9,10,11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,30,31,32,33,37,38,39,40,41,42,43,44,45,46,47,48,49,53,54,55,56,57,58,59,60,61,62,63,64)


#AccuracyDF[1,] <- c(accuracy(ARIMAmodel1), 1)

#for (i in rownames(AccuracyDF)) {
#  AccuracyDF[i,] <- c(accuracy(ARIMAmodel1))
#}

#for (i in rownames(AccuracyDF)) {
#  AccuracyDF[i,] <- c(accuracy(paste("ARIMAmodel",i, sep = '')))
#}

#print(paste("ARIMAmodel",ARIMAmodelsThatWork, sep = ''))

# Couldn't get the loops to cooperate so we'll put it together manually :)
AccuracyDF[1,] <- c(accuracy(ARIMAmodel1),1)
AccuracyDF[2,] <- c(accuracy(ARIMAmodel5),1)
AccuracyDF[3,] <- c(accuracy(ARIMAmodel6),1)
AccuracyDF[4,] <- c(accuracy(ARIMAmodel7),1)
AccuracyDF[5,] <- c(accuracy(ARIMAmodel8),1)
AccuracyDF[6,] <- c(accuracy(ARIMAmodel9),1)
AccuracyDF[7,] <- c(accuracy(ARIMAmodel10),1)
AccuracyDF[8,] <- c(accuracy(ARIMAmodel11),1)
AccuracyDF[9,] <- c(accuracy(ARIMAmodel12),1)
AccuracyDF[10,] <- c(accuracy(ARIMAmodel13),1)
AccuracyDF[11,] <- c(accuracy(ARIMAmodel14),1)
AccuracyDF[12,] <- c(accuracy(ARIMAmodel15),1)
AccuracyDF[13,] <- c(accuracy(ARIMAmodel16),1)
AccuracyDF[14,] <- c(accuracy(ARIMAmodel17),1)
AccuracyDF[15,] <- c(accuracy(ARIMAmodel21),1)
AccuracyDF[16,] <- c(accuracy(ARIMAmodel22),1)
AccuracyDF[17,] <- c(accuracy(ARIMAmodel23),1)
AccuracyDF[18,] <- c(accuracy(ARIMAmodel24),1)
AccuracyDF[19,] <- c(accuracy(ARIMAmodel25),1)
AccuracyDF[20,] <- c(accuracy(ARIMAmodel26),1)
AccuracyDF[21,] <- c(accuracy(ARIMAmodel27),1)
AccuracyDF[22,] <- c(accuracy(ARIMAmodel28),1)
AccuracyDF[23,] <- c(accuracy(ARIMAmodel29),1)
AccuracyDF[24,] <- c(accuracy(ARIMAmodel30),1)
AccuracyDF[25,] <- c(accuracy(ARIMAmodel31),1)
AccuracyDF[26,] <- c(accuracy(ARIMAmodel32),1)
AccuracyDF[27,] <- c(accuracy(ARIMAmodel33),1)
AccuracyDF[28,] <- c(accuracy(ARIMAmodel37),1)
AccuracyDF[29,] <- c(accuracy(ARIMAmodel38),1)
AccuracyDF[30,] <- c(accuracy(ARIMAmodel39),1)
AccuracyDF[31,] <- c(accuracy(ARIMAmodel40),1)
AccuracyDF[32,] <- c(accuracy(ARIMAmodel41),1)
AccuracyDF[33,] <- c(accuracy(ARIMAmodel42),1)
AccuracyDF[34,] <- c(accuracy(ARIMAmodel43),1)
AccuracyDF[35,] <- c(accuracy(ARIMAmodel44),1)
AccuracyDF[36,] <- c(accuracy(ARIMAmodel45),1)
AccuracyDF[37,] <- c(accuracy(ARIMAmodel46),1)
AccuracyDF[38,] <- c(accuracy(ARIMAmodel47),1)
AccuracyDF[39,] <- c(accuracy(ARIMAmodel48),1)
AccuracyDF[40,] <- c(accuracy(ARIMAmodel49),1)
AccuracyDF[41,] <- c(accuracy(ARIMAmodel53),1)
AccuracyDF[42,] <- c(accuracy(ARIMAmodel54),1)
AccuracyDF[43,] <- c(accuracy(ARIMAmodel55),1)
AccuracyDF[44,] <- c(accuracy(ARIMAmodel56),1)
AccuracyDF[45,] <- c(accuracy(ARIMAmodel57),1)
AccuracyDF[46,] <- c(accuracy(ARIMAmodel58),1)
AccuracyDF[47,] <- c(accuracy(ARIMAmodel59),1)
AccuracyDF[48,] <- c(accuracy(ARIMAmodel60),1)
AccuracyDF[49,] <- c(accuracy(ARIMAmodel61),1)
AccuracyDF[50,] <- c(accuracy(ARIMAmodel62),1)
AccuracyDF[51,] <- c(accuracy(ARIMAmodel63),1)
AccuracyDF[52,] <- c(accuracy(ARIMAmodel64),1)

# Change Last Column to Right ARIMA model numbers
AccuracyDF$ARIMAmodelNumber <- paste("ARIMAmodel",ARIMAmodelsThatWork, sep = '')

# Test of Best Model's Prediction
ARIMAforecast25 <- data.frame(forecast(ARIMAmodel25, 20))
ARIMAdf25 <- data.frame(test, ARIMAforecast25)
ARIMAlm25 <- lm(tail.train..20. ~ ., data = ARIMAdf25)
summary(ARIMAlm25)

