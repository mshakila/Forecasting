########## ASSIGNMENT FORECASTING - Coco cola sales

## Business Problem : To forecast coco cola sales for the next 4 quarters

library(readxl)
library(Metrics)
library(forecast)

cola <- read_xlsx("E:\\Forecasting\\CocaCola_Sales_Rawdata.xlsx")
names(cola)
head(cola) # it is quartely data from 1986 to Q2-1996
dim(cola) # 42 observations and 2 columns
class(cola)
plot(cola$Sales, type = 'l')
# this data looks like having linear trend with additive seasonality. Lets check this
# in subsequent steps.

# creating dummy vars for quarters

library(splitstackshape)
df <- cSplit(cola, "Quarter","_", drop = c('Sales','Quarter_2'))
df <- df[,-4]
df
quarter_dummy <- dummy.code(df$Quarter_1, group = NULL)
cola_data <- data.frame(cbind(cola, quarter_dummy))
head(cola_data)

cola_data['t'] <- 1:42
cola_data['t_square'] <- cola_data$t*cola_data$t
cola_data['log_sales'] <- log(cola_data$Sales)
head(cola_data)
exp(7.651791)

### Data partitioning in sequence 
train_cola <- cola_data[1:38,]
test_cola <- cola_data[39:42,]

#################  MODEL   BUILDING  ###############

########################  Linear Model ############
model_linear_cola <- lm(Sales ~ t , data = train_cola)
summary(model_linear_cola) # f-stat is significant, R-squared:  0.8117,	Adjusted R-squared:  0.8065
pred_linear_cola <- as.data.frame(predict(model_linear_cola, interval='predict',newdata=test_cola))
rmse_linear_cola <- rmse(test_cola$Sales, pred_linear_cola$fit) # 591

########################## Exponential Model ############
model_exp_cola <- lm(log_sales ~ t, data = train_cola)
summary(model_exp_cola) # f-stat is significant, R-squared:  0.8291,	Adjusted R-squared:  0.8243 
pred_exp_cola <- as.data.frame(predict(model_exp_cola,interval='predict',newdata=test_cola))
rmse_expo_cola <- rmse(test_cola$Sales, exp(pred_exp_cola$fit)) # 466

######################## Quadratic Model ############
model_quadr_cola <- lm(Sales~t+t_square, data = train_cola)
summary(model_quadr_cola) # f-stat is ginificant, R-squared:  0.8799,	Adjusted R-squared:  0.873
pred_quadr_cola <- as.data.frame(predict(model_quadr_cola, interval="predict",newdata=test_cola))
rmse_quadr_cola <- rmse(test_cola$Sales, pred_quadr_cola$fit) # 475

################## Additive Seasonality ###############
model_add_cola <- lm(Sales~Q1+Q2+Q3, data = train_cola)
summary(model_add_cola) # F-stat is not significant
pred_add_cola <- as.data.frame(predict(model_add_cola,interval='predict',newdata = test_cola))
rmse_add_cola <- rmse(test_cola$Sales, pred_add_cola$fit) # 1860
# very poor model: f-stat indicates that all Betas are zero (null hypothesis cannot be rejected)

################## Additive Seasonality with Linear Trend ###############
model_add_linear_cola <- lm(Sales~t+Q1+Q2+Q3,data=train_cola)
summary(model_add_linear_cola) # f-stat significant
pred_add_linear_cola <- as.data.frame(predict(model_add_linear_cola,interval='predict',newdata=test_cola))
rmse_add_linear_cola <- rmse(test_cola$Sales, pred_add_linear_cola$fit) # 464

################## Additive Seasonality with Quadratic Trend ###############
model_add_quadr_cola <- lm(Sales~t+t_square+Q1+Q2+Q3,data=train_cola)
summary(model_add_quadr_cola) # f-stat significant
pred_add_quadr_cola <- as.data.frame(predict(model_add_quadr_cola,interval='predict',newdata=test_cola))
rmse_add_quadr_cola <- rmse(test_cola$Sales, pred_add_quadr_cola$fit) # 301

#################### Multiplicative Seasonality ###################
model_multi_cola <- lm(log_sales~Q1+Q2+Q3,data=train_cola)
summary(model_multi_cola) # f-stat not significant
pred_multi_cola <- as.data.frame(predict(model_multi_cola,interval='predict',newdata=test_cola))
rmse_multi_cola <- rmse(test_cola$Sales, pred_multi_cola$fit) # 4680
# very poor model: f-stat indicates that all Betas are zero (null hypothesis cannot be rejected)

###################### Multiplicative Seasonality wirh linear trend ##############
# we know that multiplicative seasonality is not applicable, but let us find 
# its combo with linear and quadratic trends
model_multi_linear_cola <- lm(log_sales~t+Q1+Q2+Q3,data=train_cola)
summary(model_multi_linear_cola) # f-stat is significant,  R-squared:  0.9216,	Adjusted R-squared:  0.9121 
pred_multi_linear_cola <- as.data.frame(predict(model_multi_linear_cola,interval='predict',newdata = test_cola))
rmse_multi_linear_cola <- rmse(test_cola$Sales, exp(pred_multi_linear_cola$fit)) # 225

###################### Multiplicative Seasonality with quadratic trend ##############
model_multi_quadr_trend <- lm(log_sales~t+t_square+Q1+Q2+Q3,data=train_cola)
summary(model_multi_quadr_trend) # f-stat is significant
pred_multi_quadr_cola <- as.data.frame(predict(model_multi_quadr_trend,interval = 'predict',newdata = test_cola))
rmse_multi_quadr_cola <- rmse(test_cola$Sales, exp(pred_multi_quadr_cola$fit)) #581

### preparing RMSE table
table_rmse <- data.frame('Model'=c('rmse_linear_cola','rmse_expo_cola','rmse_quadr_cola','rmse_add_cola','rmse_add_linear_cola','rmse_add_quadr_cola','rmse_multi_cola','rmse_multi_linear_cola','rmse_multi_quadr_cola'),
                            'RMSE'=c(rmse_linear_cola,rmse_expo_cola,rmse_quadr_cola,rmse_add_cola,rmse_add_linear_cola,rmse_add_quadr_cola,rmse_multi_cola,rmse_multi_linear_cola,rmse_multi_quadr_cola))
table_rmse
# multiplicative seasonality with linear trend has the least RMSE. In the start, it
# looked like the data was following additive seasonality with linear trend. But 
# at closer look, we see slight increase in magnitude (of troughs), hence data is
# following multiplicative seasonality with linear trend as shown even by the model.

########## Building model for the eentire data
plot(cola_data$Sales, type = 'l')

# # multiplicative seasonality with linear trend has the least RMSE, so a better model.
# Hence we will build the same for the entire data.
new_model_multi_linear_cola <- lm(log_sales~t+Q1+Q2+Q3,data=cola_data)
summary(new_model_multi_linear_cola) # R-squared:  0.9404,	Adjusted R-squared:  0.934  
summary(model_multi_linear_cola) # R-squared:  0.9216,	Adjusted R-squared:  0.9121
# with new model as compared to model with only train data, there is a slight increase 
# in R-sqr and adj R-sqr

# Getting residuals
residual_cola <- residuals(new_model_multi_linear_cola)
residual_cola  
acf(residual_cola, lag.max = 6)   # lag 1,2 have significant info. choosing (4+2lags
pacf(residual_cola,lag.max = 6) # partial-acf shows that info is in only lag-1 which
# has been percolated to other lags

#### Let us now forecast the residuals
# since residuals had some significant lags, i.e., info, let us build model on errors
# to capture that info.
# let us use arima model with lag-1
model_residuals_arima <- arima(residual_cola, order=c(1,0,0))
# summary(model_residuals_arima)
acf(model_residuals_arima$residuals, lag.max = 6)
# no significant lags . both models have captured enough info (original model + residual model)  
  
# predict the data
# 1. from residual model predict for future 12 months
# 2. from original model predcit for future 12 months
# 3. final forecast is sum of these 2 predictions

# 1. from residual model predict for future 12 months
pred_res_cola <- predict(arima(residual_cola,order = c(1,0,0)),n.ahead = 4)
pred_res_cola

# 2. from original model predcit for future 12 months
# to predict we need variables like t,t-square,Jan,Feb,etc, so we need to create a new
# dataset with these variables . the same is manually created in excel sheet and we will
# use it here
library(readxl)
test_data_cola<-read_excel("E:\\Forecasting\\Cococola_new_data.xlsx",1) #Load Cocacola_new_data
head(test_data_cola)
names(test_data_cola)

pred_new_cola<-data.frame(predict(new_model_multi_linear_cola,newdata=test_data_cola,interval = 'predict'))
head(pred_new_cola)
pred_new_cola
pred_res_cola
pred_new_cola$fit <- pred_new_cola$fit+pred_res_cola$pred
pred_new_cola
# these are the predicted values for the next 4 quarters of cocacola sales

############ Exponential Smoothing to Forecast ###############

library(forecast)
library(fpp)
library(smooth)
library(tseries)

cola <- read_xlsx("E:\\Forecasting\\CocaCola_Sales_Rawdata.xlsx")
names(cola) #  "Quarter" "Sales" 
head(cola)

cola_data1 <- ts(cola$Sales, frequency = 4, start=c(86))
cola_data1
plot(cola_data1) # this shows X-axis label as years
# linear trend with multiplicative seasonality
plot(cola$Sales, type = 'l') 

train_cola1 <- cola_data1[1:38]
test_cola1 <- cola_data1[39:42]
class(train_cola1)

train_cola1 <- ts(train_cola1,frequency = 4)
test_cola1 <- ts(test_cola1, frequency = 4)

# Plotting time series data
plot(train_cola1)
# Visualization shows that it has level, trend, seasonality => 
# MUltiplicative seasonality with linear trend


#### USING HoltWinters function ################
# we will use HoltWinters technique to forecast

#  here giving all alpha, beta and gamma values, considering data has all 3, level, trend and seasonality- 
#     called triple exponential smoothing or Holt's Winter method
hw_abg <- HoltWinters(train_cola1, alpha = 0.2, beta=0.1, gamma = 0.1)
plot(forecast(hw_abg, h = 4))
# plot shows that data has captured all 3. but not precise
hw_abg_pred <- as.data.frame(predict(hw_abg, n.ahead = 4))
hwabg_mape <- mape(test_cola1, hw_abg_pred$fit)*100
hwabg_mape # 3.476227

# let us not give optimal values and let the model decide the best parameters
hw_nabg <- HoltWinters(train_cola1)
hw_nabg
# the Smoothing parameters chosen by the model are
#  alpha: 0.3784328, beta : 0.2526015 , gamma: 0.8897278
plot(forecast(hw_nabg,h=4))
# the forecasted values are very closely following the historical data

hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
hwnabg_mape<-mape(test_cola1,hwnabg_pred$fit)*100
hwnabg_mape # 2.471251, this is less than previous MAPE value
rmse(test_cola1, hwnabg_pred$fit) # 126

############ Forecasting for the entire data
# Based on the MAPE value we choose holts winter exponential tecnique which assumes the time series
# data has level, trend, seasonality : with default values of alpha, beta and gamma

new_model_holtwinter <- HoltWinters(cola_data1)
new_model_holtwinter #  alpha: 0.3963858, beta : 0.2321364, gamma: 0.9921668
plot(forecast(new_model_holtwinter,h=4))

# Forecasted values for the next 12 months (not present in historical data)
forecast_new <- data.frame(forecast(new_model_holtwinter,h=4))
forecast_new



'''
CONCLUSIONS
we have cocacola data giving details of quarterly Sales. The data is 
from first quarter of 1986 to 2nd quarter of 1996. We have to forecast cola sales for 
the next four quarters (Q3-1996 to Q2-1997)

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
Since data had all 3 (level, trend and seasonality), we first mnually gave the values of
alpha, beta and gamma. Next we allowed the model to choose these values.
when we compared results using MAPE, we found latter method giving better forecast.

We have built various forecast models, built acf and pacf plots, compared results
using RMSE or MAPE to get a better forecasting model.

'''








