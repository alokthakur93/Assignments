zoo <- read.csv(file.choose())
zoo <- zoo[-1]
View(zoo)
zoo$type <- factor(zoo$type, levels = c("1","2","3","4","5","6","7"),labels = c("categor-1",
                    "category-2","category-3","category-4","category-5","category-6","category-7"))
table(zoo$type)
round(prop.table(table(zoo$type))*100,1)
summary(zoo[c("feathers","aquatic","legs")])

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
zoo_n <- as.data.frame(lapply(zoo[1:16], norm))
View(zoo_n)

zoo_train <- zoo_n[1:80,]
zoo_test <- zoo_n[81:101,]

zoo_train_labels <- zoo[1:80,17]

zoo_test_labels <- zoo[81:101,17]

library("class")
test.acc <- NULL
train.acc <- NULL
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=i)
  train.acc <- c(train.acc,mean(train_zoo_pred==zoo_train_labels))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  test.acc <- c(test.acc,mean(test_zoo_pred==zoo_test_labels))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train.acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test.acc,type="l",main="Test_accuracy",col="red")

acc_df <- data.frame(list(train.acc=train.acc,test.acc=test.acc,neigh=seq(3,200,2)))
acc_df

library(ggplot2)
ggplot(acc_df,aes(x=neigh))+
  geom_line(aes(y=train.acc,colour="train.acc"),lwd=1.5)+
  geom_line(aes(y=test.acc,colour="test.acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train.acc","test.acc"),values = c("train.acc"="green","test.acc"="red"))

zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=3)
mean(zoo_pred==zoo_test_labels)
zoo_pred




