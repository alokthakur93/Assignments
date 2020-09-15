#Problem statement.
#Recommend a best book based on the ratings.

library("recommenderlab")
library(caTools)
#book rating data
book_rate_data <- read.csv(file.choose())
book_rate_data1 <- book_rate_data[-c(1)]
View(book_rate_data1)

class(book_rate_data1)

#metadata about the variable
str(book_rate_data1)
table(book_rate_data1$Book.Title)

#rating distribution
hist(book_rate_data1$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book_rate_data1, 'realRatingMatrix')
class(book_rate_data_matrix)

#Popularity based 
book_recomm_model1 <- Recommender(book_rate_data_matrix, method="POPULAR")
#Predictions for two users 
book_recommended_items1 <- predict(book_recomm_model1, book_rate_data_matrix[21:22], n=5)
as(book_recommended_items1, "list")

## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(book_rate_data_matrix,method="UBCF")

#Predictions for two users 
book_recommended_items2 <- predict(book_recomm_model2,book_rate_data_matrix[21:22], n=5)
as(book_recommended_items2, "list")

