#STEP 1: IMPORT LIBRARIES AND DATA
library(tseries)
library(forecast)
data = as.ts(scan("Bahar_Azadi.txt"))
data

#STEP 2: MODEL ESTIMATION
par(mfrow=c(1,3))
plot(data,main="Time Series")
acf(data, main="ACF of Gold Coin price data",lag.max=20,ylim = c(-1,1))
pacf(data,main="PACF of Gold Coin price data",lag.max=20,ylim=c(-1,1))

#STEP 3 : TESTS FOR STATIONARITY
adf.test(data)
#first order difference
data_d1 = diff(data)
adf.test(data_d1)
#plots of the first order time series
par(mfrow=c(1,3))
plot(data_d1,main="Time Series")
acf(data_d1, main="ACF of Gold Coin price data",lag.max=20,ylim = c(-1,1))
pacf(data_d1,main="PACF of Gold Coin price data",lag.max=20,ylim=c(-1,1))

#STEP 4: PARAMETER ESTIMATION
data_fit = arima(x = data, order = c(4, 1, 0))
data_fit

#STEP 5 : FITTED VALUES
fitted(data_fit)

#STEP 6 : RESIDUAL ANALYSIS
par(mfrow=c(1,3))
plot(data_fit$residuals, ylab='Residuals')
acf(data_fit$residuals,ylim=c(-1,1))
pacf(data_fit$residuals,ylim=c(-1,1))
checkresiduals(data_fit)

#STEP 7 : FORECASTING
data_predict = forecast(data_fit,h=12)
data_predict
par(mfrow = c(1,1))
plot(forecast(data_fit,h=24))
