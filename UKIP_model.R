
#Import Libraries
library(xts)
library(tidyverse)
library(tseries)
library(dygraphs)
library(forecast)
library(readr)


#Import Data
urlfile="https://raw.githubusercontent.com/raddy123/UKIP/main/uk_inflation_data.csv"
dta = read.csv(urlfile)
dta$ds = as.Date(dta$ds)

#Clean Data
inflation = ts(dta$y, frequency = 12, start = c(1998,1))

#Check Model Suitability
diffs = diff(inflation)
adf.test(diffs)
Acf(diffs)
Pacf(diffs)

#Form SARIMA Model
ukip = auto.arima(inflation)
summary(ukip)
checkresiduals(ukip)

#Generate Predictions
predictions = forecast(ukip)

#Plot Series
plot.ts(inflation)
plot.ts(predictions$fitted)

#Forecast Future Inflation
plot(predictions, 
     main = "Forecast of Inflation from UKIP Model \n SARIMA(1,1,1)(0,0,2)[12]",
     xlab = "Year",
     ylab = "Inflation Rate")
grid()
