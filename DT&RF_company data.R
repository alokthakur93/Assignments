# CAUTION  ; Please clear workspace/environment window before proceeding 
library(caret)
library(C50)
library(randomForest)
company_data <- read.csv(file.choose())
hist(company_data$Sales)
High = ifelse(company_data$Sales<=8, "No", "Yes")
CD = data.frame(company_data, High)
CD <- CD[,2:12]
View(CD)
str(CD)
#Creating data partition
inTraininglocal <- createDataPartition(CD$High,p=.75,list=F)
training <- CD[inTraininglocal,]
View(training)
testing <- CD[-inTraininglocal,]
table(testing$High)


# Building model on training data 

model <- C5.0(training$High~.,data = training)

summary(model)
plot(model)
# Training accuracy
pred_train <- predict(model,training)

mean(training$High==pred_train) 
library(caret)
confusionMatrix(pred_train,training$High)
#predicting on test data
pred <- predict.C5.0(model,testing[,-12])
table(pred)
a <- table(testing$High,pred)
a
sum(diag(a)/sum(a))
mean(pred==testing$High) 
confusionMatrix(pred,testing$High)



library(gmodels)
# Cross tablez
CrossTable(testing$High,pred)

# Building a random forest model on training data 
fit.forest <- randomForest(High~.,data=training, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(training$High==predict(fit.forest,training)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$High) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(testing$High, pred_test)


# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(fit.forest)
varImpPlot(fit.forest ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
importance(fit.forest)
