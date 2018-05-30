setwd("C:/Users/HP/Desktop/Topic_Mining")

# Install

# Load
library("tm")
library("SnowballC")
library("wordcloud")

#get the tweets
some_tweets = read.csv("rawTweets.csv")
tweets <-some_tweets$x

#Figure the most frequent words
corpus <- Corpus(VectorSource(tweets))
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(3,10))) 
N <- 30
findFreqTerms(a.dtm1, N)

m <- as.matrix(a.dtm1)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)

write.csv(findFreqTerms(a.dtm1, N), "keywords.csv")

filePath <- "paragraph2.txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 2)
write.csv(head(d,2),"keywordfromP.csv")

keywordfromT = read.csv("keywords.csv")
keywordfromT = keywordfromT$x

keywordfromP = read.csv("keywordfromP.csv")
keywordfromP = keywordfromP$word

matchcount = 0

for (wordP in keywordfromP) {
  for(wordT in keywordfromT){
    if (wordT == wordP){
      matchcount = matchcount +1
    }
  }
}

if(matchcount >= ceiling(length(keywordfromP)*.50) )
{
  print(paste("The topics are related"))
}


