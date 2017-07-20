library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(tidytext)
library(reshape)

load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
tweets<-trump_tweets_df
tweets<-tweets[,1]

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))
tweet_words

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)
nrc

docs<-Corpus(VectorSource(tweet_words))

stop<-c("#")
stop<-as.character(stop)

trans<-content_transformer(function(x,pattern) gsub(pattern," ",x))
docs<-tm_map(docs,trans,"/")
docs<-tm_map(docs,trans,"@")
docs<-tm_map(docs,trans,"\\|")
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,removeWords,stop)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)

dtm<-TermDocumentMatrix(docs)
mat<-as.matrix(dtm)
v<-sort(rowSums(mat),decreasing = TRUE)

d<-data.frame(word = names(v),freq=v)
head(d,10)

set.seed(666)
wordcloud(word=d$word,freq = d$freq,min.freq = 1,max.words = 200,random.order = FALSE, 
          rot.per = 0.35,colors=brewer.pal(8,"Dark2"))

word<-tweet_words$word

sentiment<-get_nrc_sentiment(word)
word<-cbind(word,sentiment)
View(word)

h1<-c(sum(word$anger),sum(word$anticipation),sum(word$disgust),sum(word$fear),
       sum(word$joy),sum(word$sadness),sum(word$surprise),sum(word$trust),sum(word$negative),sum(word$positive))
h2<-c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")

m<-matrix(h1, byrow=T, nrow=1)
barplot(m,names.arg=c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative","Positive"))
title(main = "Trump's Words Style Distribution", xlab = "Word Kinds", ylab = "Amounts")

