#Importing trainig salary dataset
SalaryData_Train <- read.csv(file.choose())
str(SalaryData_Train)
View(SalaryData_Train)

SalaryData_Train$educationno <- as.factor(SalaryData_Train$educationno)
class(SalaryData_Train)
#Inporting test salary dataset
SalaryData_Test <- read.csv(file.choose())
str(SalaryData_Test)
View(SalaryData_Test)
SalaryData_Test$educationno <- as.factor(SalaryData_Test$educationno)
class(SalaryData_Test)

#Visualization
library(ggplot2)
ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$age, fill = SalaryData_Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(SalaryData_Train$workclass,SalaryData_Train$Salary)

plot(SalaryData_Train$education,SalaryData_Train$Salary)

plot(SalaryData_Train$educationno,SalaryData_Train$Salary)

plot(SalaryData_Train$maritalstatus,SalaryData_Train$Salary)

plot(SalaryData_Train$occupation,SalaryData_Train$Salary)

plot(SalaryData_Train$relationship,SalaryData_Train$Salary)

plot(SalaryData_Train$race,SalaryData_Train$Salary)

plot(SalaryData_Train$sex,SalaryData_Train$Salary)

ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$capitalgain, fill =SalaryData_Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$capitalloss, fill =SalaryData_Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$hoursperweek, fill = SalaryData_Train$hoursperweek)) +
  geom_boxplot() +
  ggtitle("Box Plot")
plot(SalaryData_Train$native,SalaryData_Train$Salary)

#Density plot

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$age, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$workclass, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$education, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$educationno, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$maritalstatus, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$occupation, fill = SalaryData_Train$occupation)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$sex, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$relationship, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$race, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$capitalgain, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$capitalloss, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$hoursperweek, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=SalaryData_Train,aes(x = SalaryData_Train$native, fill = SalaryData_Train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

#Naive Bayes Model
library(e1071)
Model <- naiveBayes(SalaryData_Train$Salary ~ ., data = SalaryData_Train)
Model

Model_pred <- predict(Model,SalaryData_Test)
mean(Model_pred==SalaryData_Test$Salary)

#Confusion matrix
library(caret)

confusionMatrix(Model_pred,SalaryData_Test$Salary)






