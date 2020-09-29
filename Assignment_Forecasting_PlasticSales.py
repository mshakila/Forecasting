######## Assignment - Forecsting for Plastic Sales


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

plastic = pd.read_csv("E:/Forecasting/PlasticSales.csv")
plastic.head()
# monthly sales from Jan-1949 to Dec-1953

month =['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'] 
p = plastic["Month"][0]
p[0:3]
plastic['months']= 0

for i in range(60):
    p = plastic["Month"][i]
    plastic['months'][i]= p[0:3]
    
month_dummies = pd.DataFrame(pd.get_dummies(plastic['months']))
plastic1 = pd.concat([plastic,month_dummies],axis = 1)
plastic1.head()
plastic1["t"] = np.arange(1,61)
plastic1["t_squared"] = plastic1["t"]*plastic1["t"]
plastic1.columns
plastic1["log_sales"] = np.log(plastic1["Sales"])
plastic1.rename(columns={"Sales ": 'Sales'}, inplace=True)
plastic1.Sales.plot()
Train = plastic1.head(48)
Test = plastic1.tail(12)

####################### L I N E A R ##########################
import statsmodels.formula.api as smf 

linear_model = smf.ols('Sales~t',data=Train).fit()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(Test['t'])))
rmse_linear = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_linear))**2))
rmse_linear

##################### Exponential ##############################

Exp = smf.ols('log_sales~t',data=Train).fit()
pred_Exp = pd.Series(Exp.predict(pd.DataFrame(Test['t'])))
rmse_Exp = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(np.exp(pred_Exp)))**2))
rmse_Exp

#################### Quadratic ###############################

Quad = smf.ols('Sales~t+t_squared',data=Train).fit()
pred_Quad = pd.Series(Quad.predict(Test[["t","t_squared"]]))
rmse_Quad = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_Quad))**2))
rmse_Quad

################### Additive seasonality ########################

add_sea = smf.ols('Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea = pd.Series(add_sea.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov']]))
rmse_add_sea = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_add_sea))**2))
rmse_add_sea

################## Additive Seasonality with Linear trend ############################

add_sea_linear = smf.ols('Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea_linear = pd.Series(add_sea_linear.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_squared']]))
rmse_add_sea_linear = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_add_sea_linear))**2))
rmse_add_sea_linear  # 135, the least rmse

################## Additive Seasonality Quadratic ############################

add_sea_Quad = smf.ols('Sales~t+t_squared+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea_quad = pd.Series(add_sea_Quad.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_squared']]))
rmse_add_sea_quad = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(pred_add_sea_quad))**2))
rmse_add_sea_quad 


################## Multiplicative Seasonality ##################

Mul_sea = smf.ols('log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_sea = pd.Series(Mul_sea.predict(Test))
rmse_Mult_sea = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(np.exp(pred_Mult_sea)))**2))
rmse_Mult_sea

##################Multiplicative Seasonality with linear trend ###########

Mul_sea_linear = smf.ols('log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mul_sea_linear = pd.Series(Mul_sea_linear.predict(Test))
rmse_Mul_sea_linear = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(np.exp(pred_Mul_sea_linear)))**2))
rmse_Mul_sea_linear 

##################Multiplicative Seasonality with quadratic trend ###########

Mul_sea_quad = smf.ols('log_sales~t+t_squared+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mul_sea_quad = pd.Series(Mul_sea_quad.predict(Test))
rmse_Mul_sea_quad = np.sqrt(np.mean((np.array(Test['Sales'])-np.array(np.exp(pred_Mul_sea_quad)))**2))
rmse_Mul_sea_quad 

# from excel sheet, when we compare the rmse values we find that,
# Additive seasonality with linear trend has the least value (135).

########### predicting new data using entire dataset
predict_data = pd.read_excel("E:/Forecasting/Predict_new_plastic.xlsx")
model_full = smf.ols('Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=plastic1).fit()

pred_new  = pd.Series(model_full.predict(predict_data))
pred_new

predict_data["forecasted_Sales"] = pd.Series(pred_new)
predict_data

##################### Exponential Smoothing techniques 
from statsmodels.tsa.holtwinters import SimpleExpSmoothing , Holt,ExponentialSmoothing

# Creating a function to calculate the MAPE value for test data 
def MAPE(pred,org):
    temp = np.abs((pred-org))*100/org
    return np.mean(temp)

########################### Simple Exponential Method
ses_model = SimpleExpSmoothing(Train["Sales"]).fit()
pred_ses = ses_model.predict(start = Test.index[0],end = Test.index[-1])
MAPE(pred_ses,Test.Sales) # 17.041

############### Holt method 
holt_model = Holt(Train["Sales"]).fit()
pred_holt = holt_model.predict(start = Test.index[0],end = Test.index[-1])
MAPE(pred_holt,Test.Sales) # 101.99

###### Holts winter exponential smoothing with additive seasonality with linear trend
hw_model = ExponentialSmoothing(Train["Sales"],seasonal="add",trend="add",seasonal_periods=12,damped=True).fit()
pred_hw = hw_model.predict(start = Test.index[0],end = Test.index[-1])
MAPE(pred_hw,Test.Sales) # 14.745

##### Holts winter exponential smoothing with multiplicative seasonality and linear trend
hwe_model_mul_add = ExponentialSmoothing(Train["Sales"],seasonal="mul",trend="add",seasonal_periods=12).fit()
pred_hwe_mul_add = hwe_model_mul_add.predict(start = Test.index[0],end = Test.index[-1])
MAPE(pred_hwe_mul_add,Test.Sales) # 15

########## predict for entire data
hw_model_full = ExponentialSmoothing(plastic1["Sales"],seasonal="add",trend="add",seasonal_periods=12,damped=True).fit()
pred_hw_full = hw_model_full.predict(start = plastic1.index[0],end = plastic1.index[-1])
MAPE(pred_hw_full,plastic1.Sales) # 2.86

######### Visualization of Forecasted values for Test data set using different methods 
plt.plot(Train.index, Train["Sales"], label='Train',color="black")
plt.plot(Test.index, Test["Sales"], label='Test',color="blue")

plt.plot(pred_ses.index, pred_ses, label='SimpleExponential',color="green")
# we just get level here, hence a horizontal line

plt.plot(pred_holt.index, pred_holt, label="Holt method")
# here we have only level and trend, no seasonality

plt.plot(pred_hw.index, pred_hw, label='Holts_winter add',color="red")
# here we have all 3 - level, trend and  additive seasonality

plt.plot(pred_hwe_mul_add.index,pred_hwe_mul_add,label="HoltsWinter multi",color="brown")
# here we have all 3 - level, trend and  multiplicative seasonality


'''
CONCLUSIONS
We have tried to forecast sales of Plastic. The data has monthly entries
of 5 years. 

First we did some pre-processing, created new variables, dummy variables,
split the data into train and test (sequential splitting). 

We have used model based approaches like linear, quadratic, additive, etc.
We calculated MAPE to compare model performance and found Additive seasonality
with linear trend having least MAPE.

We have also used data driven approaches like simple, double and triple
exponential smoothing methods. Even here we found, Holt-Winter method with
additive seasonality and linear trend having least MAPE.

We have also visualized the pattern of predicted values

'''




















