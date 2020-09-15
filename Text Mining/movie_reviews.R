#Extracting reviews of Avengers:Endgame from IMDB
library(rvest)
library(XML)
library(magrittr)
aurl <- "https://www.imdb.com/title/tt4154796/reviews?ref_=tt_sa_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"avengers.txt")
length(IMDB_reviews)
getwd()

library(tm)
library(topicmodels)
library(slam)

movie <- readLines(file.choose()) #import avengers.txt
str(movie)
length(movie)
head(movie)

movie.corpus <- Corpus(VectorSource(movie))
movie.corpus <- tm_map(movie.corpus,tolower)
movie.corpus <- tm_map(movie.corpus,removePunctuation)
my_stopwords <- readLines(file.choose())
movie.corpus <- tm_map(movie.corpus,removeWords,my_stopwords)
boat.corpus <- tm_map(boat.corpus,removeNumbers)
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
movie.corpus <- tm_map(movie.corpus, content_transformer(removeURL))
movie.corpus <- tm_map(movie.corpus,stripWhitespace)
inspect(movie.corpus[5:15])
movie.corpus<-tm_map(movie.corpus,removeWords, c('movie','movies','film'))
movie.corpus<-tm_map(movie.corpus,removeWords, c('character','characters'))
movie.corpus <- tm_map(movie.corpus,stripWhitespace)

inspect(movie.corpus[20:30])

#build a term document matrix
movie.tdm <- TermDocumentMatrix(movie.corpus)

movie.dtm <- t(movie.tdm)
movie.tdm <- as.matrix(movie.tdm) 
movie.tdm[1:10,1:10]

# Bar plot
w <- rowSums(movie.tdm)
w

w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

#word cloud

library(wordcloud)
w <- sort(rowSums(movie.tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)


library(wordcloud2)

w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
#windows()

wordcloud2(w, size=0.5, shape='circle')

wordcloud2(w, size=0.5, shape = 'triangle')


#####Emotion mining
library(syuzhet)
movie.text <- readLines(file.choose()) #import avengers.txt
sv <- get_sentences(movie.text)
class(sv)
str(sv)
head(sv)

sentimental.vector <- get_sentiment(sv,method = "bing")
head(sentimental.vector)

nrcvector <- get_sentiment(sv,method = "nrc")
head(nrcvector)

sum(sentimental.vector)
mean(sentimental.vector)
summary(sentimental.vector)

#Polarity value graph

plot(sentimental.vector,type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h= 0, col= "red")

#to extract the sentence with most negative emotional valence
negative.sentiment <- sv[which.min(sentimental.vector)]
negative.sentiment
#to extract the sentence with most positive emotional valence
positive.sentiment <- sv[which.max(sentimental.vector)]
positive.sentiment

#sentimental analysis
reviews <- as.character(movie.text)
class(reviews)
s <- get_nrc_sentiment(reviews)
s
#obtain sentiment score
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for reviews')













