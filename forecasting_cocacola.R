library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(readxl)
cocacola <- read_excel(file.choose())
View(cocacola)
class(cocacola)
ccts <- ts(cocacola$Sales,frequency = 4,start=c(86))
class(ccts)
View(ccts)
train_data <- ccts[1:38] #choosing 70% as training data
test_data <- ccts[39:42]
train_data <- ts(train_data,frequency = 4)
test_data <- ts(test_data,frequency = 4)
plot(train_data)
#As data shows some anomalies we must go smoothing technique

#########Using Holtwinters function###################
hw_alpha <- HoltWinters(train_data,alpha = 0.2,beta = F,gamma = F)
hw_alpha
hwalpha_pred <- forecast(hw_alpha)
hwalpha_pred<-data.frame(predict(hw_alpha,n.ahead=4))  
plot(forecast(hw_alpha,h=4))
hwalpha_mape<-MAPE(hwalpha_pred$fit,test_data)*100
hwalpha_mape  

# with alpha = 0.2, beta = 0.1  
hw_alphabeta<-HoltWinters(train_data,alpha = 0.2,beta = 0.1,gamma = F)
hw_alphabeta
hwalphabeta_pred<-data.frame(predict(hw_alphabeta,n.ahead = 4))
plot(forecast(hw_alphabeta,h=4))
hwalphabeta_mape<-MAPE(hwalphabeta_pred$fit,test_data)*100
hwalphabeta_mape  

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_alphabetagamma<-HoltWinters(train_data,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_alphabetagamma
hwalphabetagamma_pred<-data.frame(predict(hw_alphabetagamma,n.ahead = 4))
plot(forecast(hw_alphabetagamma,h=4))
hwalphabetagamma_mape<-MAPE(hwalphabetagamma_pred$fit,test_data)*100
hwalphabetagamma_mape  

# With out optimum values 
hw_nalpha<-HoltWinters(train_data,beta = F,gamma = F)
hw_nalpha
hwnalpha_pred<-data.frame(predict(hw_nalpha,n.ahead = 4))
hwnalpha_pred
plot(forecast(hw_nalpha,h=4))
hwnalpha_mape<-MAPE(hwnalpha_pred$fit,test_data)*100
hwnalpha_mape

hw_nalphabeta<-HoltWinters(train_data,gamma=F)
hw_nalphabeta
hwnalphabeta_pred<-data.frame(predict(hw_nalphabeta,n.ahead=4))
hwnalphabeta_pred
plot(forecast(hw_nalphabeta,h=4))
hwnalphabeta_mape<-MAPE(hwnalphabeta_pred$fit,test_data)*100
hwnalphabeta_mape

hw_nalphabetagamma<-HoltWinters(train_data)
hw_nalphabetagamma
hwnalphabetagamma_pred<-data.frame(predict(hw_nalphabetagamma,n.ahead =4))
hwnalphabetagamma_pred
plot(forecast(hw_nalphabetagamma,h=4))
hwnalphabetagamma_mape<-MAPE(hwnalphabetagamma_pred$fit,test_data)*100
hwnalphabetagamma_mape  

df_mape<-data.frame(c("hwalpha_mape","hwalphabeta_mape","hwalphabetagamma_mape","hwnalpha_mape","hwnalphabeta_mape","hwnalphabetagamma_mape"),c(hwalpha_mape,hwalphabeta_mape,hwalphabetagamma_mape,hwnalpha_mape,hwnalphabeta_mape,hwnalphabetagamma_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)  
#Using model with least MAPE value  
#Final model
final_model<-HoltWinters(ccts)
final_model
final_pred<-data.frame(predict(final_model,n.ahead=4))
final_pred
plot(forecast(final_model,h=4))
final_mape<-MAPE(final_pred$fit,test_data)*100
final_mape
final_model$alpha  
final_model$beta  
final_model$gamma


#Auto.Arima model on the price agg data
library(forecast)
model_AA <- auto.arima(train_data)
model_AA
pred_AA <- data.frame(forecast(model_AA))
pred_AA
acf(model_AA$residuals)
pacf(model_AA$residuals)
plot(forecast(model_AA,h=4),xaxt="n")




