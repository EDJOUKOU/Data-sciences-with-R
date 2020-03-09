########### ARIMA Model built on Total Final energy consumption ########################
#
#
## Import and load data on the work directory##############################################

library(XLConnect)
w <-loadWorkbook(file.choose(), create = TRUE)
Shannon <- readWorksheet(w, sheet = 3, startRow = 13, startCol = 2, endRow =16, endCol = 6, header = TRUE )
View(Enerdata)

# Transform the data into a data frame

EnerdataDF= data.frame(Enerdata)

# Transform the data into time series
Fc <- ts(data = EnerdataDF[,2], start = 1990, end = 2016, frequency = 1)
Fc_train_set <- window(Fc, start = 1990, end = 2012)
Fc_test_set <- window(Fc, start = 2013)
plot(P)
P=diff(diff(Fc))


## Import necessary libraries
library(forecast)
library(caret)
library(astsa)
library(tseries)
library(quantmod)
library(DMwR)
library(fpp)

##Stationarity###################################################################
##number of differences necessary to make data stationary
ndiffs(Fc_train_set)


# Unit Root Tests
# Augmented Dickey-Fuller Test ADF
adf.test(P,alternative="stationary")
#The p-value of the above ADF test shows that the time series is strictly nonstationary. Similarly, we can confirm this by using the PP test as shown below:

# Phillips-Perron Test 
pp.test(P,alternative="stationary")
# the time series can be considered stationary

#find optimal parameters#########################################################

## ARIMA Models Specification
## Normal and Partial Autocorrelation Functions ACF & PACF
acf(P)
pacf(P)

## Model selection and evaluation#######################################################
Mod.Fc1 <- Arima(Fc_train_set, order=c(0,1,0))
summary(Mod.Fc1)
Mod.Fc2 <- Arima(Fc_train_set, order=c(0,1,1))
summary(Mod.Fc2)
Mod.Fc3 <- Arima(Fc_train_set, order=c(0,1,2))
summary(Mod.Fc3)
Mod.Fc4 <- Arima(Fc_train_set, order=c(0,1,3))
summary(Mod.Fc4)

str(summary(Mod.Fc4)) 
# Mod.Fc2 is preferred since it has the lowest AIC and the lowest BIC

# Check the model residuals
tsdisplay(residuals(Mod.Fc2), lag.max=10, main='(0,1,1) Model Residuals')
checkresiduals(Mod.Fc2)
#Thus we can confirm that the residuals are not distinguishable from a white noise series as the results are not significant

#Checking using auto.arima
modelauto <-auto.arima(Fc_train_set, seasonal=FALSE)
modelauto

#### Ljung Box Q-statistics ##############################################
Mod.Fc2 <- arima(Fc_train_set, order = c(0,1,1))
SSEFc2 <- sum(Mod.Fc2$residuals^2)
SSEFc2
Mod.Fc2.test <- Box.test(Mod.Fc2$residuals, lag = log(length(Mod.Fc2$residuals)))
Mod.Fc2.test

### Forecast of actual and test data#######################################################################
tr_ARIMA_Fc <- fitted(Mod.Fc2)
tr_ARIMA_Fc

test_ARIMA_Fc <- forecast(Mod.Fc2, h = 4, level = 95) 
test_ARIMA_Fc$upper


## performance of the ARIMA model over test set 

accmeasures_Rc= regr.eval(TFCtest$y, test_ARIMA_Fc$mean)
accmeasures_Rc

## Residuals of the model/ New time series for GSANN_SVR model###################################
residuals <- tr_ARIMA_Fc-Fc_train_set
residuals

Fc_mod <- data.frame(residuals)
Fc_mod <- data.frame(x=residuals)
Fc_mod
plot(Fc_mod)
#################################################################################################
### Energy 2030 pathways################################################################

Mo_FC <- Arima(Fc, order=c(0,1,1))
summary(Mo_FC)


# Check the model residuals
tsdisplay(residuals(Mo_FC), lag.max=10, main='(0,1,1) Model Residuals')
checkresiduals(Mo_FC)
#Thus we can confirm that the residuals are not distinguishable from a white noise series as the results are not significant


### Forecast till 2030 #######################################################################
tr_ARIMA_Fc1 <- fitted(Mo_FC)
tr_ARIMA_Fc1

pred_ARIMA_Fc <- forecast(Mo_FC, h = 14) 
pred_ARIMA_Fc


