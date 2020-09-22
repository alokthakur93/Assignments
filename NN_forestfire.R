forest <- read.csv(file.choose())
View(forest)
forest <- forest[-c(12:31)]#these columns are not needed in analysis
View(forest)
sum(is.na(forest))
length(which(forest$area==0))
# Convert month and day string variables into numeric values
forest$month <- as.numeric(as.factor(forest$month))
forest$day <- as.numeric(as.factor(forest$day))
View(forest)

windows()
pairs(forest)
cor(forest)
summary(forest)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest_norm<-as.data.frame(lapply(forest,FUN=normalize))
summary(forest_norm$area)

#Data partition
forest_train <- forest_norm[1:362,]
forest_test <- forest_norm[363:512,]

library(neuralnet)
library(nnet)
#Building model
forest_model <- neuralnet(area~.,data = forest_train)
str(forest_model)
plot(forest_model)
#Evaluating model performance
model_results <- compute(forest_model,forest_test[1:10])
predicted_profit <- model_results$net.result
predicted_profit
#since predicted values are in normalized form we have to unnormalize it
str_max <- max(forest$area)
str_min <- min(forest$area)
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
#model results
cor(predicted_profit,forest_test$area)
plot(predicted_profit,forest_test$area)
#Improving model performance

model_5<-neuralnet(area~.,data= forest_train,hidden = c(3,2))
plot(model_5)
model_5_res<-compute(model_5,forest_test[1:10])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forest_test$area)
plot(pred_strn_5,forest_test$area)





