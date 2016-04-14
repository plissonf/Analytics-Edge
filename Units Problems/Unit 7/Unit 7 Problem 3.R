# Load data
tweets = read.csv("./tweets.csv", stringsAsFactors=FALSE)
str(tweets)

# Pre-processing 
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

allTweets = DocumentTermMatrix(corpus)
allTweets # unique words == terms

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

wordcloud(colnames(allTweets),scale=c(1,0.2))


?wordcloud
colnames(allTweets)
colSums(allTweets)
