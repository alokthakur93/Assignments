startups <- read.csv(file.choose())
View(startups)
class(startups)

startups$State <- as.numeric(revalue(startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startups)
#EDA
attach(startups)
plot(R.D.Spend,Profit)

plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)
windows()
pairs(startups)
cor(startups)
summary(startups)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(startups,FUN=normalize))
summary(startups_norm$Profit)
#Data partition
startups_train <- startups_norm[1:35,]
startups_test <- startups_norm[36:50,]

library(neuralnet)
library(nnet)
#Building model
startups_model <- neuralnet(Profit~.,data = startups_train)
str(startups_model)
plot(startups_model)
#Evaluating model performance
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result
predicted_profit
#since predicted values are in normalized form we have to unnormalize it
str_max <- max(startups$Profit)
str_min <- min(startups$Profit)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
#model results
cor(predicted_profit,startups_test$Profit)
plot(predicted_profit,startups_test$Profit)
#Improving model performance

model_5<-neuralnet(Profit~.,data= startups_train,hidden = c(3,2))
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startups_test$Profit)
plot(pred_strn_5,startups_test$Profit)









