library(readxl)
airlines <- read_excel(file.choose())
View(airlines)#Seasonality 12 months
#so creating 12 dummy variables
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
colnames(X)<-month.abb # Assigning month names 
View(X)
airdata<-cbind(airlines,X)
View(airdata)
colnames(airdata)
plot(airlines$Passengers, type = "o")
airdata["t"]<-c(1:96)

airdata["log_Passengers"]<-log(airdata["Passengers"])
airdata["t_square"]<-airdata["t"]*airdata["t"]
attach(airdata)
train<-airdata[1:67,]
test<-airdata[68:96,]

###############---------<LINEAR MODEL>----------##############

linear_model <- lm(Passengers~t,data = train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

###############---------<EXPONENTIAL MODEL>------##############

expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Multiplicative Additive seasonality have least RMSE

write.csv(airdata,file="airdata.csv",col.names = F,row.names = F)
getwd()

####################### Predicting new data #############################

final_data<-read.csv("airdata.csv")
View(final_data)
pred_new<-data.frame(predict(multi_add_sea_model,newdata=final_data,interval = 'predict'))
new_model_fin <- exp(pred_new$fit)
View(new_model_fin)

Month <- as.data.frame(airlines$Month)
Final <- as.data.frame(cbind(Month,airdata$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)
#plot(Final$New_Pred_Value,type="o")
##############---------------FORECAST USING AUTO ARIMA----------##################
library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(readxl)


class(airlines)
airlines1 <- ts(airlines$Passengers,start=c(1995,1),end=c(2002,12),frequency=12)
View(airlines1)
summary(airlines1)

decomdata<- decompose(airlines1, "multiplicative")
plot(decomdata)

#EDA on original data
plot(airlines1)
abline(reg=lm(airlines1~time(airlines1)))

cycle(airlines1)

# Boxplot by Cycle
boxplot(airlines1~cycle(airlines1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))

# Use Auto Arima for the Best Model


#Auto.Arima model 
library(forecast)
model_AA <- auto.arima(airlines1)
model_AA

acf(model_AA$residuals)
pacf(model_AA$residuals)
Pass_Forecast <- forecast(model_AA,Level=c(95),h=5*12) #forecast for next 5 years
Pass_Forecast
plot(Pass_Forecast)


