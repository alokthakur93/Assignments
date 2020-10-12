library(forecast)
library(fpp)
library(smooth)
library(tseries)
plastic <- read.csv(file.choose())
View(plastic)
class(plastic)
pts <- ts(plastic$Sales,frequency = 12,start=c(49))
class(pts)
View(pts)
train <- pts[1:48] #choosing 80% as training data
test <- pts[49:60]
train <- ts(train,frequency = 12)
test <- ts(test,frequency = 12)
plot(train)


#########Using Holtwinters function###################
hw_a <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred <- forecast(hw_a)
hwa_pred<-data.frame(predict(hw_a,n.ahead=12))  
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape  

# with alpha = 0.2, beta = 0.1

hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape  

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

#model with least MAPE is selected

#Final model
final_model <- HoltWinters(pts)
plot(forecast(final_model))
final_pred<-data.frame(predict(pts,h=12))
final_pred
final_new<-MAPE(final_pred$Point.Forecast,test)*100
final_new
final_model$gamma


#Auto.Arima model 
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))
pred_AA
acf(model_AA$residuals)
pacf(model_AA$residuals)
plot(forecast(model_AA,h=12),xaxt="n")


























