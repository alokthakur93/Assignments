sms <- read.csv(file.choose())
View(sms)
class(sms)
str(sms)
sms$type <- factor(sms$type)
str(sms)
table(sms$type)


library(tm)
# Prepare corpuse for the text data 
sms.corpous<-Corpus(VectorSource(sms$text))

# Cleaning data (removing unwanted symbols)
corpus.clean<-tm_map(sms.corpous,tolower)
corpus.clean<-tm_map(corpus.clean, removeNumbers)
corpus.clean<-tm_map(corpus.clean,removeWords, stopwords())
corpus.clean<-tm_map(corpus.clean,removePunctuation)
corpus.clean<-tm_map(corpus.clean,stripWhitespace)
class(corpus.clean)
as.character(corpus.clean)

sms.dtm <- DocumentTermMatrix(corpus.clean) 
class(sms.dtm)
# creating training and test datasets
sms_raw_train <- sms[1:4169, ]
sms_raw_test  <- sms[4170:5559, ]

sms_dtm_train <- sms.dtm[1:4169, ]
sms_dtm_test  <- sms.dtm[4170:5559, ]

sms_corpus_train <- corpus.clean[1:4169]
sms_corpus_test  <- corpus.clean[4170:5559]
# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 5)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_train
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_dict
inspect(sms_corpus_train[1:100])
list(sms_dict[1:100])

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- as.data.frame(apply(sms_test, MARGIN = 2, convert_counts))
View(sms_train)
View(sms_test)
##  Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
class(sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy 
mean(sms_test_pred==sms_raw_test$type)

