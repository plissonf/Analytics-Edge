# Read in the data

emails <- read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)

table(emails$spam)

# Install new packages

library(tm)
library(SnowballC)

# Create corpus
corpus = Corpus(VectorSource(emails$text))


# Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus= tm_map(corpus, PlainTextDocument)

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)


# Remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))


# Stem document 
corpus = tm_map(corpus, stemDocument)


# Create matrix
dtm = DocumentTermMatrix(corpus)
dtm

length(stopwords("english"))

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# Convert to a data frame
emailsSparse = as.data.frame(as.matrix(spdtm))

# Make all variable names R-friendly
colnames(emailsSparse) = make.names(colnames(emailsSparse))

# Merge dtms
dtm = cbind(dtmTitle, dtmAbstract)

# Add dependent variable
emailsSparse$spam = emails$spam

#Convert dependent variable
emailsSparse$spam = as.factor(emailsSparse$spam)

# Split the data

library(caTools)

set.seed(123)

spamsplit = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

# Build logistic regression model
spamLog = glm(spam ~., data=train, family=binomial)

# Build a CART model

library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data=train, method="class")
prp(spamCART)

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
