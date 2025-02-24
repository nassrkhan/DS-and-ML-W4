#Time series analysis with R

#Use the ts() function to plot some time series data
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)

#Now add a regression line to study the trend:
plot.ts(birthstimeseries)
abline(reg=lm(birthstimeseries~time(birthstimeseries)))

#simple moving average
install.packages("TTR")
library("TTR")
birthstimeseries <- SMA(birthstimeseries, n=15)
plot.ts(birthstimeseries)

#Seasonality Trend Decomposition (STD)
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

#Try filtering out the random signal

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries-birthstimeseriescomponents$random
plot.ts(birthstimeseriesseasonallyadjusted)

#Auto-correlation where the lag=1
#birthstimeseries <- na.omit(birthstimeseries)
plot(birthstimeseries[2:168], birthstimeseries[1:167])
cor(birthstimeseries[2:168], birthstimeseries[1:167], use="complete.obs")

#compute the auto-correlation for 1-24 lags
acf( birthstimeseries, lag.max=24 )

#the partial auto-correlation
pacf(birthstimeseries, lag.max=24)

#Augmented dickey fuller test for stationarity
install.packages("tseries")
library("tseries")
adf.test(birthstimeseries)

plot.ts( diff(birthstimeseries) )

# ARIMA modelling using R
install.packages("forecast")
library(forecast)
modelfit <- auto.arima(AirPassengers)
forecasts <- forecast( modelfit, h=50 )
plot( forecasts )

#Evaluate the accuracy of the model
accuracy(forecasts)

#9 years of training data
AirPassengersTrain <- ts(AirPassengers[1:108], frequency=12, start=c(1949,1))
#3 years of testing data
AirPassengersTest <- ts(AirPassengers[109:144], frequency=12, start=c(1958,1))
modelfit <- auto.arima(AirPassengersTrain)
forecasts <- forecast( modelfit, h=36 )
accuracy(f = forecasts, x = AirPassengersTest)
plot( AirPassengersTest, col="red", ylab="values")
lines(forecasts$mean, col="blue")
