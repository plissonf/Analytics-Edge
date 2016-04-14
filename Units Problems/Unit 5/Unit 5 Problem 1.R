# Read in the data

wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)

table(wiki$Vandal)
wiki$Vandal = as.factor(wiki$Vandal)

# Install new packages

library(tm)
library(SnowballC)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusRemoved = Corpus(VectorSource(wiki$Removed))

# Convert to lower-case
corpusAdded = tm_map(corpusAdded, tolower)
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
corpusRemoved = tm_map(corpusRemoved, tolower)
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

# Remove punctuation
corpusAdded = tm_map(corpusAdded, removePunctuation)
corpusRemoved = tm_map(corpusRemoved, removePunctuation)

# Remove stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

# Stem document 
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusRemoved = tm_map(corpusRemoved, stemDocument)


# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
length(stopwords("english"))

# Remove sparse terms
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Convert to a data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("A", colnames(wordsRemoved))

# Make all variable names R-friendly
colnames(wordsAdded) = make.names(colnames(wordsAdded))
colnames(wordsRemoved) = make.names(colnames(wordsRemoved))

# Merge dtms
wikiWords = cbind(wordsAdded, wordsRemoved)

# Add dependent variable
wikiWords$Vandal = wiki$Vandal

#Convert dependent variable
wikiWords$Vandal = as.factor(wikiWords$Vandal)

# Split the data

library(caTools)

set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

vantrain = subset(wikiWords, split==TRUE)
vantest = subset(wikiWords, split==FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART3)

# New models
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

# Build a RF model

library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

# Predictions models

predictLog = predict(spamLog, newdata=train)
predictCART = predict(spamCART, newdata=train, type="class")
predictRF = predict(spamRF, newdata=train)
# Predicted probabilities models
#training set
predLog = predict(spamLog, newdata=train)
predCART = predict(spamCART, newdata=train)
probCART = predCART[,2]
predRF = predict(spamRF, newdata=train, type="prob")
probRF = predRF[,2]

#testing set
predLog2 = predict(spamLog, newdata=test)
predCART2 = predict(spamCART, newdata=test)
probCART2 = predCART2[,2]
predRF2 = predict(spamRF, newdata=test, type="prob")
probRF2 = predRF2[,2]

# Compute ROC

library(ROCR)
predROCR = prediction(predLog2, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values
