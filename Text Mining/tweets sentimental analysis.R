#Text mining from twitter
library("twitteR")
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='oOKlUQaGjuxX9cKTIMKsGf3yY', # Consumer Key (API Key)
                         consumerSecret='KCWIOWMgYgv3RJj4rUrcXcYBX9ZAiSqSPVeF85WcxRaqq55qiL', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

library(base64enc)

library(httpuv)

setup_twitter_oauth("oOKlUQaGjuxX9cKTIMKsGf3yY", # Consumer Key (API Key)
                    "KCWIOWMgYgv3RJj4rUrcXcYBX9ZAiSqSPVeF85WcxRaqq55qiL", #Consumer Secret (API Secret)
                    "1303970115143127040-KOcB9zl7syb7Rh0Cbwr2phgAzSiMLJ",  # Access Token
                    "nJx1ZGBvY2Djud8x5Iy2S9MhBSUUGwb9tKxPFyI2BjxaN")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('realDonaldTrump', n = 3000,includeRts = T)

TweetsDF <- twListToDF(Tweets)
View(TweetsDF)
write.csv(TweetsDF, "Tweets.csv")

getwd()
#Using extracted tweets
library(tm)
library(topicmodels)
library(slam)

x <- read.csv(file.choose()) #Tweets.csv
str(x)
text <- as.character(x$text)
View(text)
length(text)
#using tm package#

text.corpus <- Corpus(VectorSource(text))
inspect(text.corpus[1:5])
text.corpus <- tm_map(text.corpus,removePunctuation)
inspect(text.corpus[1:5])
my_stopwords <- readLines(file.choose())
text.corpus <- tm_map(text.corpus,removeWords,my_stopwords)
text.corpus <- tm_map(text.corpus,removeNumbers)
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
text.corpus <- tm_map(text.corpus, content_transformer(removeURL))
text.corpus <- tm_map(text.corpus,stripWhitespace)
inspect(text.corpus[1:5])

#build a term document matrix
text.tdm <- TermDocumentMatrix(text.corpus)

text.dtm <- t(text.tdm)
text.tdm <- as.matrix(text.tdm) 
text.tdm[1:10,1:10]

# Bar plot
words <- rowSums(text.tdm)
words

words_sub <- subset(words, words >= 20)
words_sub

barplot(words_sub, las=2, col = rainbow(30))
#word clouds
words

words_small <- subset(words, words >= 10)
words_small

barplot(words_small, las=2, col = rainbow(30))


library(wordcloud)
w <- sort(rowSums(text.tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)

words1 <- data.frame(names(words_small), words_small)
colnames(words1) <- c('word', 'freq')
#windows()

wordcloud2(words1, size=0.5, shape='circle')
?wordcloud2

wordcloud2(words1, size=0.5, shape = 'triangle')

#####Emotion mining
library(syuzhet)
tweetdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(tweetdata$text)
class(tweets)

sen_vec <- get_sentences(tweets)
class(sen_vec)
str(sen_vec)
head(sen_vec)

sentimental_vector <- get_sentiment(sen_vec,method = "bing")
head(sentimental_vector)

nrc.vector <- get_sentiment(sen_vec,method = "nrc")
head(nrc.vector)

#Polarity value graph

plot(sentimental_vector,type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h= 0, col= "red")

#to extract the sentance with most negative emotional valence
negative_sentiment <- sen_vec[which.min(sentimental_vector)]
negative_sentiment
#"the vaccine, destroy the suburbs, erase your borders, 
#and indoctrinate your children with poisonous anti-American lies!"

# and to extract most positive sentence
positive_sentiment <- sen_vec[which.max(sentimental_vector)]
positive_sentiment
#"RT @BuckSexton: Strong covid briefing by President Trump \n\nSerious, 
#fact based, calm but in command \n\nEven the journos were reasonably well."
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
#A vote for Republicans is a vote for safe communities, great jobs, 
#and a limitless future for ALL Americans. Instead of letting Washington change us, 
#despite all that we have been through, we are changing Washington!
#The above tweet have mixture of anger,anticipation,joy,sadness,surprise,trus,negative&positive
get_nrc_sentiment('safe')
#safe have mixture of anger,joy,trust,positive sentiments

#barplot
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Twitter Tweets')


