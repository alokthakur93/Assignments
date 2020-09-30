credit <- read.csv(file.choose())
View(credit)
credit <- credit[,-1]
str(credit)
model <- glm(card~.,data=credit,family = "binomial")
summary(model)
exp(coef(model))
prob <- predict(model,credit,type="response")
confusion<-table(prob>0.5,credit$card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 90.1
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
credit[,"prob"] <- prob
credit[,"pred_values"] <- pred_values
credit[,"yes_no"] <- yes_no
table(credit$card,credit$pred_values)
View(credit[,c(1,13:15)])
library(ROCR)
rocrpred<-prediction(prob,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


