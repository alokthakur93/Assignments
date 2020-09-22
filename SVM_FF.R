FF <- read.csv(file.choose())
View(FF)
class(FF)
str(FF)

#the area value has lots of zero
hist(FF$area)

#Transforming the area value
library(plyr)
FF1 <- mutate(FF, y = log(area + 1))  # default is to the base e, y is lower case
hist(FF1$y)

summary(FF) #normalization needed here 

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
#prediction of forest fires requires prediction using only temperature,relative humidity,
#wind and rain. so applying normalization to only these instead of whole data set

FF$temp = normalize(FF$temp)
FF$RH   = normalize(FF$RH)
FF$wind = normalize(FF$wind)
FF$rain = normalize(FF$rain)

table(FF$size_category)

#Data Partition
FF_train <- FF[1:362,]
FF_test <- FF[363:517,]

# Building model 

library(kernlab)
library(caret)

# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH,data = FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)#65.8


#kernel = vanilladot
model_rfdot<-ksvm(size_category~temp+rain+wind+RH,data = FF_train,kernel = "vanilladot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)#67.1

# kernal = besseldot
model_rfdot<-ksvm(size_category~temp+rain+wind+RH,data = FF_train,kernel = "besseldot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)#67.1

# kernel = polydot
model_rfdot<-ksvm(size_category~temp+rain+wind+RH,data = FF_train,kernel = "polydot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category)#67.1




