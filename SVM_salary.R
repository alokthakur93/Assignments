Salary_train <- read.csv(file.choose())
str(Salary_train)
View(Salary_train)
Salary_train$educationno <- as.factor(Salary_train$educationno)
class(Salary_train)

Salary_test <- read.csv(file.choose())
str(Salary_test)
Salary_test$educationno <- as.factor(Salary_test$educationno)
class(Salary_test)

#Building model

# kernel = rfdot 
model_rfdot<-ksvm(Salary_train$Salary~.,data = Salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=Salary_test)
mean(pred_rfdot==Salary_test$Salary)#85.2

# kernel = vanilladot
model_rfdot<-ksvm(Salary_train$Salary~.,data = Salary_train,kernel = "vanilladot")
pred_rfdot<-predict(model_rfdot,newdata=Salary_test)
mean(pred_rfdot==Salary_test$Salary)#84.64
