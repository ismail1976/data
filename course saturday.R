###*************************************************************************
###*************************************************************************
###                                                                      ***
###                    machine learning in time series                   ***
###                                                                      ***
###                                                                      ***
###*************************************************************************
###*************************************************************************
library(readxl)
Exer13_5_data <- read_excel("C:/Users/lenovo/Desktop/Exer13_5_data.xls")
View(Exer13_5_data)
df<-Exer13_5_data
gdp<-as.vector(df[[3]])
gdp<-ts(gdp,frequency = 4,start = c(1947,1))
plot(gdp)
decgdp<-decompose(gdp)
decgdp
plot(decgdp)
#### Plot the Different Components Individually
plot(decgdp$trend)
plot(decgdp$seasonal)
plot(decgdp$random)
###*************************************************************************
###                                                                      ***
###                                                                      ***
###                     1- prediction by ARIMA                           ***
###                                                                      ***
###*************************************************************************
###*************************************************************************

###      SPLITTING THE DATA TRAINING AND TEST SETS
###   in sample 
train_set<-ts(gdp,frequency = 4,start = c(1947,1),end = c(2000,4))
test_set<-ts(gdp,frequency = 4,start = c(2001,1),end = c(2010,4))
train_set
####        study stationnarty
acf(train_set)
pacf(train_set)
library(tseries)
library(forecast)
adf.test(train_set)
PP.test(train_set)
dtrain_set<-diff(train_set,differences = 1)
plot(dtrain_set)
adf.test(dtrain_set)
PP.test(dtrain_set)
acf(dtrain_set)
pacf(dtrain_set)
ddtrain_set<-diff(train_set,differences = 2)
plot(ddtrain_set)
adf.test(ddtrain_set)
PP.test(ddtrain_set)
# Training the model ------------------------------------------------------

model1<-auto.arima(ddtrain_set)
model1


###*  *** Prediction ***
#     --------------------

predtest_set <- forecast(model1,h=40) 
predtest_set
plot(predtest_set)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set
mae <- Metrics::mae(actual = actual, predicted = predtest_set)
mse <- Metrics::mse(actual = actual, predicted = predtest_set)
rmse <- Metrics::rmse(actual = actual, predicted = predtest_set)

# Table of results

knitr::kable(cbind(mae, mse, rmse))
#### out sample:
train_set1<-gdp
####        study stationnarty
acf(train_set1)
pacf(train_set1)
library(tseries)
library(forecast)
adf.test(train_set1)
PP.test(train_set1)
dtrain_set1<-diff(train_set1,differences = 1)
plot(dtrain_set1)
adf.test(dtrain_set1)
PP.test(dtrain_set1)
acf(dtrain_set1)
pacf(dtrain_set1)
ddtrain_set1<-diff(train_set1,differences = 2)
plot(ddtrain_set1)
adf.test(ddtrain_set1)
PP.test(ddtrain_set1)
# Training the model ------------------------------------------------------

model2<-auto.arima(ddtrain_set1)
model2


###*  *** Prediction ***
#     --------------------

predtest_set1 <- forecast(model2,h=10) 
predtest_set1
plot(predtest_set1)
###*************************************************************************
###                                                                      ***
###                                                                      ***
###                    2- Prediction by exponenial smoothing             ***
###                                                                      ***
###*************************************************************************

###      SPLITTING THE DATA TRAINING AND TEST SETS
train_set<-ts(gdp,frequency = 4,start = c(1947,1),end = c(2000,4))
# Training the model ------------------------------------------------------
args(HoltWinters)
model3<-HoltWinters(train_set)
model3
test_set<-ts(gdp,frequency = 4,start = c(2001,1),end = c(2010,4))
###*  *** Prediction ***
#     --------------------

predtest_set2 <- forecast(model3,h=40) 
predtest_set2
plot(predtest_set2)

###       **** Model Evaluation ***
#         -------------------------
actual <- test_set
mae <- Metrics::mae(actual = actual, predicted = predtest_set2)
mse <- Metrics::mse(actual = actual, predicted = predtest_set2)
rmse <- Metrics::rmse(actual = actual, predicted = predtest_set2)

# Table of results

knitr::kable(cbind(mae, mse, rmse))
