# CAUTION  ; Please clear workspace/environment window before proceeding 
library(caret)
library(C50)
Fraud_check <- read.csv(file.choose())
hist(Fraud_check$Taxable.Income)
Risky_Good = ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(Fraud_check,Risky_Good)
FC <- FC[,-3]
View(FC)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(FC$Risky_Good,p=.75,list=F)
training <- FC[inTraininglocal,]
View(training)
testing <- FC[-inTraininglocal,]
table(testing$Risky_Good)

# Building model on training data 

model <- C5.0(training$Risky_Good~.,data = training)
summary(model)
plot(model)

# Training accuracy
pred_train <- predict(model,training)
mean(training$Risky_Good==pred_train) 
library(caret)
confusionMatrix(pred_train,training$Risky_Good)
#predicting on test data
pred <- predict.C5.0(model,testing[,-6])
table(pred)
a <- table(testing$Risky_Good,pred)
a
sum(diag(a)/sum(a))
mean(pred==testing$Risky_Good) 
confusionMatrix(pred,testing$Risky_Good)

library(gmodels)
# Cross tablez
CrossTable(testing$Risky_Good,pred)

library(randomForest)
# Building a random forest model on training data 
fit.forest <- randomForest(Risky_Good~.,data=training, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(training$Risky_Good==predict(fit.forest,training)) 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=testing)
mean(pred_test==testing$Risky_Good)
library(gmodels)
# Cross table 
rf_perf<-CrossTable(testing$Risky_Good, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)

varImpPlot(fit.forest)
varImpPlot(fit.forest ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
importance(fit.forest)









