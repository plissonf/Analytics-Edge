# Read in the data

trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)

# Install new packages

library(tm)
library(SnowballC)

# Create corpus
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

# Convert to lower-case
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

# Remove stopwords
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem document 
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

# Create matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract

# Remove sparse terms
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract 

# Convert to a data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

length(stopwords("english"))

# Differentiate dtms
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# Make all variable names R-friendly
colnames(dtmTitle) = make.names(colnames(dtmTitle))
colnames(dtmAbstract) = make.names(colnames(dtmAbstract))

# Merge dtms
dtm = cbind(dtmTitle, dtmAbstract)

# Add dependent variable
dtm$trial = trials$trial

# Split the data

library(caTools)

set.seed(123)

trialsplit = sample.split(dtm$trial, SplitRatio = 0.7)

trialtrain = subset(dtm, split==TRUE)
trialtest = subset(dtm, split==FALSE)

# Build a CART model

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=trialtrain, method="class")
prp(trialCART)

# Evaluate the performance of the model

predictCART = predict(trialCART, newdata=trialtest, type="class")
table(dtm$trial, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)