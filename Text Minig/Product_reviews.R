#Extracting revies of Boat Basshead 100 earphone from amazon
library(rvest)
library(XML)
library(magrittr)
aurl <- "https://www.amazon.in/product-reviews/B071Z8M4KX/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber=1#reviews-filter=all_reviews"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"boat.txt")
length(amazon_reviews)
getwd()

library(tm)
library(topicmodels)
library(slam)

boat.earphone <- readLines(file.choose()) #import boat.txt
str(boat.earphone)
length(boat.earphone)
head(boat.earphone)

boat.corpus <- Corpus(VectorSource(boat.earphone))
boat.corpus <- tm_map(boat.corpus,removePunctuation)
my_stopwords <- readLines(file.choose())
boat.corpus <- tm_map(boat.corpus,removeWords,my_stopwords)
boat.corpus <- tm_map(boat.corpus,removeNumbers)
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
boat.corpus <- tm_map(boat.corpus, content_transformer(removeURL))
boat.corpus <- tm_map(boat.corpus,stripWhitespace)
inspect(boat.corpus[10:20])
boat.corpus<-tm_map(boat.corpus,removeWords, c('earphone'))

#build a term document matrix
boat.tdm <- TermDocumentMatrix(boat.corpus)

boat.dtm <- t(boat.tdm)
boat.tdm <- as.matrix(boat.tdm) 
boat.tdm[1:10,1:20]


# Bar plot
w <- rowSums(boat.tdm)
w

w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

#word cloud

library(wordcloud)
w <- sort(rowSums(boat.tdm), decreasing = TRUE) # Sort words in decreasing order.
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
?wordcloud2

wordcloud2(w, size=0.5, shape = 'triangle')


#####Emotion mining
library(syuzhet)
boat.text <- readLines(file.choose()) #import boat.txt
sv <- get_sentences(boat.text)
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

#to extract the sentance with most negative emotional valence
negative.sentiment <- sv[which.min(sentimental.vector)]
negative.sentiment
#Also the audio via the microphone is bad, and the mic is located on the 
#split end of the cable, which is very low...
positive.sentiment <- sv[which.max(sentimental.vector)]
positive.sentiment

#sentimental analysis
reviews <- as.character(boat.text)
class(reviews)
s <- get_nrc_sentiment(reviews)

#obtain sentiment score
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for reviews')











