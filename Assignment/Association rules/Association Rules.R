#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visulize the obtained rules using different plots 

#------------------------Books Dataset----------------------------#

library(arules)
library(arulesViz)
books <- read.csv(file.choose())
View(books)
class(books)
books_trans<-as(as.matrix(books),"transactions")
inspect(books_trans[1:100])
summary(books_trans)
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 

#Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase 
#3 books
rules<-apriori(books_trans,parameter = list(support=0.02,confidence=0.5,minlen=3))
rules
inspect(head(sort(rules, by = "lift")))
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")

#Provided the rules with 2 % Support, 70 % Confidence and Minimum to purchase 
#5 books
rules1<-apriori(books_trans,parameter = list(support=0.02,confidence=0.7,minlen=5))
rules1
inspect(head(sort(rules1, by = "lift")))
plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")

#Provided the rules with Support = 0.002, 60 % Confidence and Minimum to purchase 
#4 books
rules2<-apriori(books_trans,parameter = list(support=0.002,confidence=0.6,minlen=4))
rules2
inspect(head(sort(rules2, by = "lift")))
plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")

#Provided the rules with Support = 0.0021, 90 % Confidence and Minimum to purchase 
#3 books
rules3<-apriori(books_trans,parameter = list(support=0.0021,confidence=0.9,minlen=3))
rules3
inspect(head(sort(rules3, by = "lift")))
plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")

####Combinations with highest lift is recommended to frame any marketing startegy to 
#increase the sales



####--------------------Groceries Dataset----------------------############

library(arules)
library(arulesViz)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
summary(groceries)


#support = 0.002 and confidence = 50% without any minimum purchase
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.5))

  
groceries_rules
inspect(head(sort(groceries_rules, by = "lift")))
plot(groceries_rules)
plot(groceries_rules, method = "grouped")
plot(groceries_rules, method = "graph")

#support = 0.001 and confidence = 50% with minimum purchase of 4

groceries_rules1<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.5,minlen=4))
  
groceries_rules1
inspect(head(sort(groceries_rules1, by = "lift")))
plot(groceries_rules1)
plot(groceries_rules1, method = "grouped")
plot(groceries_rules1, method = "graph")

##support = 0.001 and confidence = 70% with minimum purchase of 3
groceries_rules2<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.7,minlen=3))
  
groceries_rules2
inspect(head(sort(groceries_rules2, by = "lift")))
plot(groceries_rules2)
plot(groceries_rules2, method = "grouped")
plot(groceries_rules2, method = "graph")

##support = 0.002 and confidence = 70% with minimum purchse of 3

groceries_rules3<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.7,minlen=3))

groceries_rules3
inspect(head(sort(groceries_rules3, by = "lift")))
plot(groceries_rules3)
plot(groceries_rules3, method = "grouped")
plot(groceries_rules3, method = "graph")

####Combinations with highest lift is recommended to frame any marketing startegy to 
#increase ths sales


##########------------------MY_MOVIES DATASET------------------############


mymovies <- read.csv(file.choose())

View(mymovies)

movierules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
movierules

inspect(head(sort(movierules, by = "lift")))

plot(movierules)
plot(movierules, method = "grouped")
plot(movierules, method = "graph")

movierules1 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.02, confidence = 0.7,minlen=3)))
movierules1

inspect(head(sort(movierules1, by = "lift")))
plot(movierules1)


##Getting exact same rules although parameters have been changed

movierules2 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.02, confidence = 0.5,minlen=3)))
movierules2

##Again same rules, there is no change in rules despite of different parameters

movierules3 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.02, confidence = 0.7,minlen=5)))
movierules3
