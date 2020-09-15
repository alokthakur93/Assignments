#Read the dataset
glass <- read.csv(file.choose())
View(glass)

table(glass$Type)
#Replacing the type with their respective names
glass$Type <- factor(glass$Type, levels = c("1","2","3","5","6","7"), 
                     labels = c("building_windows_float_processed",
                                "building_windows_non_float_processed",
                                "vehicle_windows_float_processed","containers","tableware","headlamps"))
str(glass)
#To know how much % of respective glass type present
round(prop.table(table(glass$Type))*100,1) 

#Function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)

set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]


#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]




# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test.acc <- NULL
train.acc <- NULL

for(i in seq(3,200,2))
{
  train.glass.pred <- knn(train = glass_train,test = glass_train, cl=glass_train_labels,k=i)
  train.acc <- c(train.acc,mean(train.glass.pred==glass_train_labels))
  teat.glass.pred <- knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=i)
  test.acc <- c(test.acc,mean(teat.glass.pred==glass_test_labels))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train.acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test.acc,type="l",main="Test_accuracy",col="red")

#glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
#table(glass_test_pred,glass_test_labels)
#mean(glass_test_pred==glass_test_labels)


acc.df <- data.frame(list(train.acc=train.acc,test.acc=test.acc,neigh=seq(3,200,2)))
acc.df



library(ggplot2)
ggplot(acc.df,aes(x=neigh))+
  geom_line(aes(y=train.acc,colour="train.acc"),lwd=1.5)+
  geom_line(aes(y=test.acc,colour="test.acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train.acc","test.acc"),values = c("train.acc"="green","test.acc"="red"))



glass.pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=13)
mean(glass.pred==glass_test_labels)


















