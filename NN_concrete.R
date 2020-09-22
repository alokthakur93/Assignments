concrete <- read.csv(file.choose())
View(concrete)
str(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
View(concrete_norm)
summary(concrete_norm$strength)


concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

library(neuralnet)
library(nnet)

# Building model
concrete_model <- neuralnet(strength~.,data = concrete_train)
str(concrete_model)
plot(concrete_model)

# Evaluating model performance
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result
predicted_strength

cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)

# New model
model_5<-neuralnet(strength~.,data= concrete_norm,hidden = c(5,3))
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)
plot(pred_strn_5,concrete_test$strength)

# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased


