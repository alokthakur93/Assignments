Bank_Data <- read.csv("C://Users//LENOVO//Desktop//ExcelR//Data Science Assignments//Logistic Regression//bank-full.csv",sep = ';') 
attach(Bank_Data)
sum(is.na(Bank_Data))
library(psych)
describe(Bank_Data)
library(plyr)
Bank_Data1 <- Bank_Data
Bank_Data1$job <- as.numeric(revalue(Bank_Data1$job))
Bank_Data1$marital <- as.numeric(revalue(Bank_Data1$marital))
Bank_Data1$education <- as.numeric(revalue(Bank_Data1$education))
Bank_Data1$default <- as.numeric(revalue(Bank_Data1$default))
Bank_Data1$housing <- as.numeric(revalue(Bank_Data1$housing))
Bank_Data1$loan <- as.numeric(revalue(Bank_Data1$loan))
Bank_Data1$contact <- as.numeric(revalue(Bank_Data1$contact))
Bank_Data1$month <- as.numeric(revalue(Bank_Data1$month))
Bank_Data1$poutcome <- as.numeric(revalue(Bank_Data1$poutcome))
Bank_Data1$y <- as.numeric(revalue(Bank_Data1$y))

# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model_bank <- glm(y~.,data=Bank_Data,family = "binomial")
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model_bank))
# Confusion matrix table 
prob1 <- predict(model_bank,Bank_Data,type="response")
summary(model_bank)
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob1>0.5,Bank_Data$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
# Creating empty vectors to store predicted classes based on threshold value
pred_values1 <- NULL
yes_no1 <- NULL

pred_values1 <- ifelse(prob1>=0.5,1,0)
yes_no1 <- ifelse(prob1>=0.5,"yes","no")
Bank_Data[,"prob1"] <- prob1
Bank_Data[,"pred_vales1"] <- pred_values1
Bank_Data[,"yes_no1"] <- yes_no1
View(Bank_Data[,c(17,18:20)])

table(Bank_Data$y,Bank_Data$pred_vales1)





library(ROCR)
rocrpred1<-prediction(prob1,Bank_Data$y)
rocrperf1<-performance(rocrpred1,'tpr','fpr')

str(rocrperf1)

plot(rocrperf1,colorize=T,text.adj=c(-0.2,1.7))



str(rocrperf1)
rocr_cutoff1 <- data.frame(cut_off = rocrperf1@alpha.values[[1]],fpr=rocrperf1@x.values,tpr=rocrperf1@y.values)
colnames(rocr_cutoff1) <- c("cut_off","FPR","TPR")
View(rocr_cutoff1)



library(dplyr)
rocr_cutoff1$cut_off <- round(rocr_cutoff1$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff1 <- arrange(rocr_cutoff1,desc(TPR))
View(rocr_cutoff1)

