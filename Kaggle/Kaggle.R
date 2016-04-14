## KAGGLE IPAD COMPETITION
## Code by Fabien Plisson http://www.fabienplisson.com

setwd("./~")

# Load the datasets
## Adding in the argument stringsAsFactors=FALSE, since we have some text fields
iPadTrain = read.csv("./eBayiPadTrain.csv", stringsAsFactors=FALSE)
summary(iPadTrain)
str(iPadTrain)

iPadTest = read.csv("./eBayiPadTest.csv", stringsAsFactors=FALSE)
summary(iPadTest)
str(iPadTest)

# Dealing with Text Data
## Create corpus from the Description variable (training set and test set together)
library(tm)
library(NLP)
CorpusDescription = Corpus(VectorSource(c(iPadTrain$description, iPadTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, stopwords("english"), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

## Convert corpus to a DocumentTermMatrix, remove sparse terms (specific threshold 0.99), and turn it into a data frame.
## Check variable names are OK with make.names()
dtm = DocumentTermMatrix(CorpusDescription)
sparse = removeSparseTerms(dtm, 0.99) # 0.95-0.99 has been changed
DescriptionWords = as.data.frame(as.matrix(sparse))
colnames(DescriptionWords) = make.names(colnames(DescriptionWords))

# Split the dataset into training and test sets using head() and tail()
## The head function takes the first "n" rows of DescriptionWords (the first argument to the head function), where "n" is specified by the second argument to the head function.
## Take the first nrow(iPadTrain) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTrain"

## The tail function takes the last "n" rows of DescriptionWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
## Take the last nrow(iPadTest) observations from DescriptionWords, and putting them in a new data frame called "DescriptionWordsTest"
DescriptionWordsTrain = head(DescriptionWords, nrow(iPadTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(iPadTest))

# Before building models, add back other variables (sold, WordCount) to new datasets from old ones
DescriptionWordsTrain$sold = iPadTrain$sold

DescriptionWordsTrain$WordCount = iPadTrain$WordCount
DescriptionWordsTest$WordCount = iPadTest$WordCount


# Models
# 1. Simple logistic regression model "SimpleLog"
SimpleMod = glm(sold ~ startprice, data=iPadTrain, family=binomial)
summary(SimpleMod)

PredTrain= predict(SimpleMod, type="response")
table(iPadTrain$sold, PredTrain>=0.5)

PredTest = predict(SimpleMod, newdata=iPadTest, type="response")
summary(PredTest)

# 2. Logistic regression model "DescriptionLog" with all words from initial "description" independent variable
DescriptionWordsLog = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)
summary(DescriptionWordsLog)

PredTrain2= predict(DescriptionWordsLog, type="response")
table(DescriptionWordsTrain$sold, PredTrain2>=0.5)

PredTest2 = predict(DescriptionWordsLog, newdata=DescriptionWordsTest, type="response")
summary(PredTest2)

# 3. Logistic regression model "Mod" with independent variables (biddable + startprice + productline + UniqueID)
Mod = glm(sold ~ biddable + startprice + productline + UniqueID, data=iPadTrain, family=binomial)
summary(Mod)

PredTrain3= predict(Mod, type="response")
table(iPadTrain$sold, PredTrain3>=0.6)

PredTest3 = predict(Mod, newdata=iPadTest, type="response")
summary(PredTest3)

# 4. Logistic regression model "Mod2" with independent variables (biddable + startprice + productline)
Mod2 = glm(sold ~ biddable + startprice + productline, data=iPadTrain, family=binomial)
summary(Mod2)

PredTrain3= predict(Mod2, type="response")
table(iPadTrain$sold, PredTrain3>=0.6)

PredTest3 = predict(Mod2, newdata=iPadTest, type="response")
summary(PredTest3)

# 5. and 6. Logistic regression models
## "Log2" (biddable, startprice, productline and description(words counts), UniqueID)
## "Log4" (biddable, startprice, productline and description(words counts))
## First transfer other variables (biddable, startprice, productline) to new datasets
DescriptionWordsTrain$biddable = iPadTrain$biddable
DescriptionWordsTest$biddable = iPadTest$biddable
DescriptionWordsTrain$startprice = iPadTrain$startprice
DescriptionWordsTest$startprice = iPadTest$startprice
DescriptionWordsTrain$productline = iPadTrain$productline
DescriptionWordsTest$productline = iPadTest$productline

DescriptionWordsLog4 = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)
summary(DescriptionWordsLog4)

PredTrain6= predict(DescriptionWordsLog4, type="response")
table(DescriptionWordsTrain$sold, PredTrain5>=0.6)

PredTest6 = predict(DescriptionWordsLog4, newdata=DescriptionWordsTest, type="response")
summary(PredTest6)

## 7. Adding 5 other independent variables (condition, cellular, carrier, color, storage) to datasets
# and reevaluate Logistic regression model "Log3" (vs Log2) and "Log5" (vs Log4)
DescriptionWordsTrain$condition = iPadTrain$condition
DescriptionWordsTest$condition = iPadTest$condition
DescriptionWordsTrain$cellular = iPadTrain$cellular
DescriptionWordsTest$cellular = iPadTest$cellular
DescriptionWordsTrain$color = iPadTrain$color
DescriptionWordsTest$color = iPadTest$color
DescriptionWordsTrain$carrier = iPadTrain$carrier
DescriptionWordsTest$carrier = iPadTest$carrier
DescriptionWordsTrain$storage = iPadTrain$storage
DescriptionWordsTest$storage = iPadTest$storage

DescriptionWordsLog5 = glm(sold ~ ., data=DescriptionWordsTrain, family=binomial)
summary(DescriptionWordsLog5)

PredTrain7= predict(DescriptionWordsLog5, type="response")
table(DescriptionWordsTrain$sold, PredTrain7>=0.6)

PredTest7 = predict(DescriptionWordsLog5, newdata=DescriptionWordsTest, type="response")
summary(PredTest7)

# 8. CART model "Cart1" using 8 independent variables (except Unique ID))
library(rpart)
library(rpart.plot)
tree1 = rpart(sold ~ biddable + startprice + productline + condition + color + cellular + carrier + storage, data=iPadTrain, method="class")
prp(tree1)

PredTrain8 = predict(tree1, data=iPadTrain, type="class")
table(iPadTrain$sold, PredTrain8)
PredTest8 = predict(tree1, newdata=iPadTest) # Make predictions

## 9. CART model with Cross-validation using 8 variables + cross-validation
set.seed(201)
library(ggplot2)
library(lattice)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

train(sold ~ biddable + startprice + productline + condition + color + cellular + carrier + storage, data = iPadTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

tree1cv = rpart(sold ~ biddable + startprice + productline + condition + color + cellular + carrier + storage, data=iPadTrain, cp=0.01) # minbucket, cp values?
prp(tree1cv)
PredTrain9 = predict(tree1cv)
iPadTrain$sold = as.factor(iPadTrain$sold)
table(iPadTrain$sold, PredTrain9)
PredTest9 = predict(tree1cv, newdata=iPadTest) 
### Issue Outcomen with 3 columns (class, 0.75, 0.25) ? => 2 only!

# 10. Random Forests model
install.packages("randomForest")
library(randomForest)

iPadTrain$productline = as.factor(iPadTrain$productline) 
iPadTrain$condition = as.factor(iPadTrain$condition) 
iPadTrain$carrier = as.factor(iPadTrain$carrier) 
iPadTrain$color = as.factor(iPadTrain$color) 
iPadTrain$cellular = as.factor(iPadTrain$cellular) 
iPadTrain$storage = as.factor(iPadTrain$storage) 

iPadTest$productline = as.factor(iPadTest$productline) 
iPadTest$condition = as.factor(iPadTest$condition) 
iPadTest$carrier = as.factor(iPadTest$carrier) 
iPadTest$color = as.factor(iPadTest$color) 
iPadTest$cellular = as.factor(iPadTest$cellular) 
iPadTest$storage = as.factor(iPadTest$storage) 

levels(iPadTest$productline) = levels(iPadTrain$productline)
levels(iPadTest$condition) = levels(iPadTrain$condition)
levels(iPadTest$color) = levels(iPadTrain$color)
levels(iPadTest$carrier) = levels(iPadTrain$carrier)
levels(iPadTest$cellular) = levels(iPadTrain$cellular)
levels(iPadTest$storage) = levels(iPadTrain$storage)


forest1 = randomForest(sold ~ biddable + startprice + productline + condition + color + cellular + carrier + storage, data = iPadTrain, ntree=200, nodesize=25)

PredTrain10 = predict(forest1)
table(iPadTrain$sold, PredTrain10)

PredTest10 = predict(forest1, newdata=iPadTest)

## CV on logistic regression
numFolds = trainControl(method = "cv", number = 5)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))

iPadTrain$sold = factor(iPadTrain$sold)
train(sold ~ biddable + startprice + productline + condition + color + cellular + carrier + storage, data = iPadTrain, method = "glm", family="binomial", trControl = numFolds, tuneGrid = cpGrid)

## Insert new feature = average price per model
averageprice <- tapply(iPadTrain$startprice, iPadTrain$productline, mean)
averageprice = as.data.frame(averageprice)
averageprice$productline = rownames(averageprice)
iPadTrain = merge(averageprice, iPadTrain, by="productline")
iPadTrain$deviation = iPadTrain$startprice - iPadTrain$averageprice
iPadTrain$distance = abs(iPadTrain$startprice - iPadTrain$averageprice)

averageprice2 <- tapply(iPadTest$startprice, iPadTest$productline, mean)
averageprice2 = as.data.frame(averageprice2)
averageprice2$productline = rownames(averageprice2)
iPadTest2 = merge(averageprice, iPadTest, by="productline")
#iPadTest2 = iPadTest2[order(UniqueID),]
iPadTest2$deviation = iPadTest2$startprice - iPadTest2$averageprice
iPadTest2$distance = abs(iPadTest2$startprice - iPadTest2$averageprice)

# 11. Logistic regression using deviation feature and others.
Log = glm(sold ~ deviation + startprice + biddable + condition + color + carrier + storage, data=iPadTrain2, family=binomial)
summary(Log)

PredTrain11= predict(Log, type="response")
table(iPadTrain2$sold, PredTrain11>=0.5)

PredTest11 = predict(Log, newdata=iPadTest2, type="response")
summary(PredTest11)

# Cluster-and-predict
iPadTrainMatrix = as.matrix(iPadTrain, na.rm=TRUE)
iPadTrainVector = as.vector(iPadTrainMatrix)
iPadTestMatrix = as.matrix(iPadTest, na.rm=TRUE)
iPadTestVector = as.vector(iPadTestMatrix)

distTrain = dist(iPadTrainVector, method = "euclidean")
distTest = dist(iPadTestVector, method = "euclidean")

clusterIntensity = hclust(distTrain, method="ward.D2")


# Choosing the best threshold for models using ROC curve and AUC score
install.packages("ROCR")
library(ROCR)
library(gplots)
ROCRpred = prediction(PredTrain11, iPadTrain2$sold) # Prediction function
ROCRperf = performance(ROCRpred, "tpr", "fpr") # Performance function
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Plot ROC curve
as.numeric(performance(ROCRpred, "auc")@y.values)


# Submitted results
MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)
Logistic regression (sold ~ stateprice)
Accuracy (training set) = 0.7135948
Accuracy (test set) = 0.75188

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest2)
write.csv(MySubmission, "SubmissionDescriptionLog.csv", row.names=FALSE)
Logistic regression (sold ~.)
Accuracy (training set) = 0.586244 (sparse 0.99) / 0.5550779 (0.97) / 0.5513165 (0.95)
Accuracy (test set) = 0.60341

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest3)
write.csv(MySubmission, "SubmissionLog1.csv", row.names=FALSE)
Logistic regression (sold ~ biddable + startprice + productline)
Accuracy (training set) = 0.793122 (t=0.5) / 0.808705 (0.6)
Accuracy (test set) = 0.83587  (t=0.5) / 0.82983 (0.6)
AUC (training set) = 0.8595904 (t=0.5) / 0.8570128 (0.6)

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest4)
write.csv(MySubmission, "SubmissionLog2.csv", row.names=FALSE)
Logistic regression (sold ~ biddable + startprice + productline + UniqueID + description(wordscount))
Accuracy (training set) = 0.8081677
Accuracy (test set) = 0.82960

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest5)
write.csv(MySubmission, "SubmissionLog3.csv", row.names=FALSE)
Logistic regression (sold ~ description(wordscount) + all other independent variables)
Accuracy (training set) = 0.8167652
Accuracy (test set) = 0.83329

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest6)
write.csv(MySubmission, "SubmissionLog4.csv", row.names=FALSE)
Logistic regression (sold ~ biddable + startprice + productline + description(wordscount))
Accuracy (training set) = 0.8178399
Accuracy (test set) = 0.82420
AUC (training set) = 0.8802802

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest7)
write.csv(MySubmission, "SubmissionLog5.csv", row.names=FALSE)
Logistic regression (sold ~ 8 other variables + description(wordscount))
Accuracy (training set) = 0.8183772
Accuracy (test set) = 0.83023
AUC (training set) = 0.8884389

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest3)
write.csv(MySubmission, "SubmissionLog6.csv", row.names=FALSE)
Logistic regression (sold ~ biddable + startprice + productline)
Accuracy (training set) = 0.8027942 (t=0.6)
Accuracy (test set) = TBD
AUC (training set) = 0.8570128

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest8)
write.csv(MySubmission, "SubmissionCart1.csv", row.names=FALSE)
CART (sold ~ 8 independent variables -description(wordscount))
Accuracy (training set) = 0.8017195
Accuracy (test set) = TBD
AUC (training set) = TBD

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest9)
write.csv(MySubmission, "SubmissionCart2.csv", row.names=FALSE)
CART+CV(sold ~ 8 independent variables -description(wordscount))
Accuracy (training set) = 0.7759269
Accuracy (test set) = TBD
AUC (training set) = TBD

MySubmission = data.frame(UniqueID = iPadTest$UniqueID, Probability1 = PredTest10)
write.csv(MySubmission, "SubmissionForest1.csv", row.names=FALSE)
RF(sold ~ 8 independent variables -description(wordscount))
Accuracy (training set) = 0.8135411
Accuracy (test set) = 0.75015
AUC (training set) = TBD

MySubmission = data.frame(UniqueID = iPadTest2$UniqueID, Probability1 = PredTest11)
write.csv(MySubmission, "SubmissionDevLog.csv", row.names=FALSE)
Log = glm(sold ~ deviation + biddable + condition)
Accuracy (training set) = 0.8022569 (0.6) / 0.8113917 (0.7)
Accuracy (test set) = 0.82716   / 
AUC (training set) = 0.8551785  / 

MySubmission = data.frame(UniqueID = iPadTest2$UniqueID, Probability1 = PredTest11)
write.csv(MySubmission, "SubmissionDevLog2.csv", row.names=FALSE)
Log = glm(sold ~ deviation + startprice + biddable + condition + color + carrier + storage)
Accuracy (training set) = 0.8124664 (0.7)
Accuracy (test set) = 0.83271
AUC (training set) = 0.861733




# TO DO LIST

# Try CART, RF and cluster-then-predict (HC,km). Tune parameters for each type of model e.g nsize, ntree, ...
# Feature engineering









