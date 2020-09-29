########## ASSIGNMENT FORECASTING - Airlines-data

## Business Problem : To forecast passengers count for the next year 

# importing required libraries
library(readxl)
library(Metrics)

air <- read_xlsx("E:\\Forecasting\\Airlines_Data.xlsx")
names(air) #  "Month"      "Passengers"
head(air)
class(air)
View(air)
# starts from Jan-1995 to Dec-2002. Has 96 observations. 8 years data monthly

plot(air$Passengers, type = 'l')
# the data has level, linear trend with multiplicative seasonality

# creating dummy variables
dummy_vars <- data.frame(outer(rep(month.abb, length=96), month.abb, "==") + 0) 
# one hot encoding is done. 
colnames(dummy_vars) <- month.abb # assigning names of months
head(dummy_vars)

air_data <- cbind(air, dummy_vars)
head(air_data)

# creating variable 't'
air_data["t"] <- 1:96
# creating variable 't_square'
air_data["t_square"] <- air_data$t * air_data$t
# creating variable log_Passengers
air_data["log_passengers"] <- log(air_data["Passengers"])
log(112)
log(118)
head(air_data)


### Data partitioning in sequence 
# since time series data cannot do random splitting, has to be in sequence and 
# test data should have one full cycle. Here sinc it is monthly data, need atleast 12 obs
train_air <- air_data[1:84,] # have till Dec-2001
test_air <- air_data[85:96,] # frm jan-2002 to Dec-2002

############## MODEL BUILDING ###################

########################     LINEAR MODEL     ###################
linear_model_air <- lm(Passengers ~ t, data=train_air)
summary(linear_model_air) # R-squared:  0.7923,	Adjusted R-squared:  0.7898 
# t is statistically significant (p-value<0)
linear_pred_air <- data.frame(predict(linear_model_air, interval='predict', newdata=test_air))
View(linear_pred_air)
rmse_linear_air <- rmse(test_air$Passengers, linear_pred_air$fit) # 53.20

######################## EXPONENTIAL MODEL #######################
expo_model_air <- lm(log_passengers ~ t, data=train_air)
summary(expo_model_air) # Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8218 
expo_pred_air <- data.frame(predict(expo_model_air, interval='predict', newdata=test_air))
rmse_expo_air <- rmse(test_air$Passengers, exp(expo_pred_air$fit)) # 46.05736  
rmse_expo_air
########################### QUADRATIC MODEL ###################
quad_model_air <- lm(Passengers ~ t + t_square, data=train_air)
summary(quad_model_air) #  R-squared:  0.7963,	Adjusted R-squared:  0.7912 
quad_pred_air <- data.frame(predict(quad_model_air, interval='predict', newdata=test_air))
rmse_quad_air <- rmse(test_air$Passengers, quad_pred_air$fit) # 48.05

####################### ADDITIVE SEASONALITY #####################
sea_add_model_air <-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(sea_add_model_air) #f-stat not significant, R-squared:  0.1674,	Adjusted R-squared:  0.04015 
sea_add_pred_air <-data.frame(predict(sea_add_model_air,newdata=test_air,interval='predict'))
rmse_sea_add_air <-rmse(test_air$Passengers,sea_add_pred_air$fit) # 132
# this is a poor model as adj R-sqr is very less, f-stat is non-significant

######################## Additive Seasonality with Linear #################
Add_sea_Linear_model_air <-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(Add_sea_Linear_model_air) # R-squared:  0.9551,	Adjusted R-squared:  0.9475 
Add_sea_Linear_pred_air <-data.frame(predict(Add_sea_Linear_model_air,interval='predict',newdata=test_air))
rmse_Add_sea_Linear_air <-rmse(test_air$Passengers,Add_sea_Linear_pred_air$fit) #35

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model_air <-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(Add_sea_Quad_model_air) # R-squared:  0.9598,	Adjusted R-squared:  0.9524 
Add_sea_Quad_pred_air <-data.frame(predict(Add_sea_Quad_model_air,interval='predict',newdata=test_air))
rmse_Add_sea_Quad_air <-rmse(test_air$Passengers,Add_sea_Quad_pred_air$fit) #26
rmse_Add_sea_Quad_air # 26.36082 this is the least rmse value obtained

######################## Multiplicative Seasonality #########################

multi_sea_model_air <-lm(log_passengers ~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_air)
summary(multi_sea_model_air) # f-stat not significant,R-squared:  0.1548,	Adjusted R-squared:  0.02568
multi_sea_pred_air <-data.frame(predict(multi_sea_model_air,newdata=test_air,interval='predict'))
rmse_multi_sea_air <-rmse(test_air$Passengers,exp(multi_sea_pred_air$fit)) # # 140 
rmse_multi_sea_air
# this is a poor model 

######################## Multiplicative Seasonality with Linear trend ##########################

multi_sea_lin_model_air <-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_air)
summary(multi_sea_lin_model_air)  #  R-squared:  0.9763,	Adjusted R-squared:  0.9723 
multi_sea_lin_pred_air <-data.frame(predict(multi_sea_lin_model_air ,newdata=test_air,interval='predict'))
rmse_multi_sea_lin_air <-rmse(test_air$Passengers,exp(multi_sea_lin_pred_air$fit)) # 10.51917 
rmse_multi_sea_lin_air

######################## Multiplicative Seasonality with Quadratic trend ##########################
multi_sea_quad_model_air <- lm(log_passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(multi_sea_quad_model_air) # R-squared:  0.9777,	Adjusted R-squared:  0.9736 
multi_sea_quad_pred_air <- data.frame(predict(multi_sea_quad_model_air, newdata=test_air,interval='predict'))
rmse_multi_sea_quad_air <- rmse(test_air$Passengers, exp(multi_sea_quad_pred_air$fit)) # 18.37201 
rmse_multi_sea_quad_air
########################################

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame('Model'=c("rmse_linear_air","rmse_expo_air","rmse_Quad_air","rmse_sea_add_air","rmse_Add_sea_Linear_air","rmse_Add_sea_Quad_air","rmse_multi_sea_air","rmse_multi_sea_lin_air","rmse_multi_sea_quad_air"),'RMSE'=c(rmse_linear_air,rmse_expo_air,rmse_quad_air,rmse_sea_add_air,rmse_Add_sea_Linear_air,rmse_Add_sea_Quad_air,rmse_multi_sea_air,rmse_multi_sea_lin_air,rmse_multi_sea_quad_air))

colnames(table_rmse)<-c("model","RMSE")
# View(table_rmse)
table_rmse
# rmse_Add_sea_Quad_air with 26 has the least RMSE value

# now for this airlines data (train and test), additive-seasonality with quadratic trend is the best model. 
# next we will build model for entire dataset.
# on the errors, build ACF plot, see if it has any information
# if errors have significant information , then build separate model using errors, 
# called as error model.
# uisng main model do forecast and using error do forecast. Then combine both models.
# This final forecast would be adjusted for the error. This model is more accurate
# than the two individual models (main and error models)

############## Building model for entire data ###############

# acf, showed the model to follow linear trend and also it had multiplicative seasonality
plot(air_data$Passengers, type='l')
# The RMSE obtained i.e., 10.51917 for this model for train data was the least 
# also adj R-sqr was very hifh i.e., 97.23%

# Multiplicative-seasonality with Linear trend is the best model, so build this model
new_model_air <- lm(log_passengers ~ t ++Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=air_data)
summary(new_model_air)

# R-sqr is 98.29% and Adj R-sqr is 98.04% . F-stat is significant
# with new model as compared to model with only train data, there is a slight increase 
# in R-sqr and adj R-sqr

# Getting residuals 
resid_air <- residuals(new_model_air) # extracting errors frm new model
resid_air
acf(resid_air, lag.max = 12)
# there are many significant lags, indicating that the errors still has information
# lags 1,2,3,3,5 and 6 have significant info

# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 

# Building Autoregressive model on residuals consider lag-1 
# To capture info from errors building ARIMA model (Auto Regressive Interated Moving Average)
arima_errors_air <- arima(resid_air, order = c(1,0,0))
acf(arima_errors_air$residuals, lag.max = 12)
# now errors of these (residuals) do not have significant info indicating that the
# model has captured all info

pred_res_air <- predict(arima(resid_air,order=c(1,0,0)), n.ahead = 12)
pred_res_air
str(pred_res_air)
pred_res_air$pred
acf(arima_errors_air$residuals)
#write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################

# first predict using main model. Then add predictions of residuals. This gives final predictions
# we have to create a dataset which contains all variables like t, t_sqr, dummy var
# except Y or Passengers count, which we will be predictiong now

library(readxl)
test_data_air<-read_excel("E:\\Forecasting\\Airlines_new_data.xlsx",1) #Load Airlines_new_data

View(test_data_air)
names(test_data_air)

pred_new_air<-data.frame(predict(new_model_air,newdata=test_data_air,interval = 'predict'))
View(pred_new_air)
pred_new_air
pred_res_air
pred_new_air$fit <- pred_new_air$fit+pred_res_air$pred
pred_new_air

############ Exponential Smoothing to Forecast ###############

library(forecast)
library(fpp)
library(smooth)
library(tseries)

air <- read_xlsx("E:\\Forecasting\\Airlines_Data.xlsx")
names(air) #  "Month"      "Passengers"
head(air)

air_data1 <- ts(air$Passengers, frequency = 12, start = c(95))
air_data1
plot(air_data1) # this shows X-axis label as years
# linear trend with multiplicative seasonality
plot(air$Passengers, type = 'l') # this X-axis shows obs from 1 to 120 

train_air1 <- air_data1[1:84]
test_air1 <- air_data1[85:96]

train_air1 <- ts(train_air1, frequency = 12)
test_air1 <- ts(test_air1, frequency = 12)

# Plotting time series data

plot(train_air1)
# Visualization shows that it has level, trend, seasonality => 
# MUltiplicative seasonality with linear trend


#### USING HoltWinters function ################
# we will use HoltWinters technique to forecast

# 1.  here giving only alpha value, considering data has only level - 
#     like simple exponential smoothing
hw_a<-HoltWinters(train_air1,alpha = 0.2,beta = F,gamma = F)
plot(forecast(hw_a, h=12))
# since using only level which is average , we are getting 12 equal forecasted values
hw_a_pred <- as.data.frame(predict(hw_a, n.ahead = 12))
hwa_mape <- MAPE(test_air1, hw_a_pred$fit)*100
hwa_mape # 13.41127

# 2.  here giving both alpha and beta values, considering data has both level and trend - 
#     like double exponential smoothing or Holt's method
hw_ab<-HoltWinters(train_air1,alpha = 0.2,beta = 0.1,gamma = F)
plot(forecast(hw_ab, h=12))
# we can see a trend line sans seasonality
hw_ab_pred <- as.data.frame(predict(hw_ab, n.ahead = 12))
hwab_mape <- MAPE(test_air1, hw_ab_pred$fit)*100
hwab_mape # 10.59747

# 3.  here giving all alpha, beta and gamma values, considering data has all 3, level, trend and seasonality- 
#     like triple exponential smoothing or Holt's Winter method
hw_abg <- HoltWinters(train_air1, alpha = 0.2, beta=0.1, gamma = 0.1)
plot(forecast(hw_abg, h = 12))
# plot shows that data has captured all 3. but still can be improved
hw_abg_pred <- as.data.frame(predict(hw_abg, n.ahead = 12))
hwabg_mape <- MAPE(test_air1, hw_abg_pred$fit)*100
hwabg_mape # 6.311894

# 4. let us not give optimal values and let the model decide the best parameters
hw_nabg <- HoltWinters(train_air1)
hw_nabg
# the Smoothing parameters chosen by the model are
# alpha: 0.2294643, beta : 0.04451169, gamma: 1
plot(forecast(hw_nabg,h=12))
# the forecasted values are very closely following the historical data

hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw, h=12))
hwnabg_mape<-MAPE(test_air1,hwnabg_pred$fit)*100
hwnabg_mape # 1.678281, this is the least MAPE value
rmse(test_air1, hwnabg_pred$fit) # 9.037404

# Comparing all MAPE values
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwnabg_mape"),
                    c(hwa_mape,hwab_mape,hwabg_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
df_mape

############ Forecasting for the entire data
# Based on the MAPE value we choose holts winter exponential tecnique which assumes the time series
# data has level, trend, seasonality : with default values of alpha, beta and gamma

new_model_holtwinter <- HoltWinters(air_data1)
new_model_holtwinter #  alpha: 0.2287362, beta : 0.05161695, gamma: 1
plot(forecast(new_model_holtwinter,h=12))

# Forecasted values for the next 12 months (not present in historical data)
forecast_new <- data.frame(forecast(new_model_holtwinter,h=12))
forecast_new

#################### ARIMA MODEL #######################
library(readxl)
library(forecast)
# let us import data, visualize , convert to time-series and visualize it
air <- read_xlsx("E:\\Forecasting\\Airlines_Data.xlsx")
air_data1 <- ts(air$Passengers, frequency = 12, start = c(95))
head(air_data1)
plot(air_data1) # linear trend with multiplicative seasonality
plot(air$Passengers, type = 'l') # this X-axis shows obs from 1 to 120 
train_air1 <- air_data1[1:84]
test_air1 <- air_data1[85:96] # not converting to ts as difficulty during finding MAPE
train_air1 <- ts(train_air1, frequency = 12)
# test_air1 <- ts(test_air1, frequency = 12)
plot(train_air1)

acf(train_air1) # all non-zero lags and almost all significant
pacf(train_air1) # 2 significant lags
# so using AR model (auto-regressive), auto.arima func will decide on its own
model_arima <- auto.arima(train_air1)
model_arima
# ARIMA(0,1,1)(1,1,0)[12]
plot(forecast(model_arima, h=12), xaxt="n") # forecasted values are near to historical values

arima_train_pred<-data.frame(predict(model_arima,n.ahead =12))
arima_train_pred
class(arima_train_pred$pred)
class(test_air1)
library(Metrics)
mape(test_air1, arima_train_pred$pred)*100  # value is 2.48378

####### forecasting for entire data
model_arima_full <- auto.arima(air_data1)
model_arima_full # ARIMA(1,1,0)(1,1,0)[12]
plot(forecast(model_arima_full, h=12))

acf(model_arima_full$residuals) # 1 significant lag, so further forecast for these errors
pacf(model_arima_full$residuals) # only 1 significant lag

# Forecasted values for the next 12 months (not present in historical data)
forecast_new <- data.frame(forecast(new_model_holtwinter,h=12))
forecast_new

# To capture info from errors building ARIMA model 
model_arima_errors <- auto.arima(residuals(model_arima_full))
acf(model_arima_errors$residuals, lag.max = 12)
pacf(model_arima_errors$residuals, lag.max = 12)
# even after forecasting using only errors, one significant lag is persistent.
# we will go with forecasts of model-arima-full itself
forecast_arima <- as.data.frame(forecast(model_arima_full,h=12))
forecast_arima


'''
CONCLUSIONS
we have airlines data giving details of passenger count per month. The data is 
from 1995 to 2002 for all months. We have to forecast per month the number of passengers
who may board the flight during 2003.

We found that the data is following multiplicative seasonality with linear trend.
We have used model based approach to build the model on train data. We used linear
trend, quadratic, exponential, additive seasonality, etc methods to forecast test
data. Then we compared RMSE values of all methods and found multiplicative 
seasonality with linear trend to have the least RMSE value. We zeroed on this method.
Then we combinedboth test and train data, to build a new model using multiplicative 
seasonality with linear trend. We looked at errors ifor any information. 
Since errors had significant lags, hence had info, we forecasted errors using
ARIMA model. Now when we checked errors of these errors for information,there were 
no significant lags. 
Finally, We combined forecasts obtained from the data points and their errors. 
This combined predictions showed better forecast.

We also used exponential smoothing technique, the Holts-Winter method to forecast.
We used simple, double and triple exponential smoothing methods. Compared results
using MAPE and found the Holt-winter method to give better forecast.

We also forecasted using ARIMA model for the entire data.

We have built various forecast models, built acf and pacf plots, compared results
using RMSE or MAPE to get a better forecasting model.

'''
