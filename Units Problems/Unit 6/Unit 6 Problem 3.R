
# Load the data
stocks <- read.csv("./StocksCluster.csv")
str(stocks)

# Proportion of best return in December
table(stocks$PositiveDec)

# Correlation between variables
cor(stocks)

# Compare mean values
summary(stocks)

# Initial logistic regression
## Install and load caTools package
install.packages("caTools")
library(caTools)

## Create split, training and test sets
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#Confusiom matrix with threshold 0.5 for training set
StocksModel = glm(PositiveDec ~., data=stocksTrain, family=binomial)
predictTrain = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, predictTrain > 0.5)

#Confusiom matrix with threshold 0.5 for test set
StocksModel = glm(PositiveDec ~., data=stocksTrain, family=binomial)
predictTest = predict(StocksModel, type="response", newdata=stocksTest)
table(stocksTest$PositiveDec, predictTest > 0.5)

# Clustering - remove $PositiveDec variable from datasets
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# Pre-processing and normalization of the data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

# Mean values
summary(normTrain$ReturnJan)

# Clustering
k=3
set.seed(144)
km = kmeans(normTrain, centers = k, iter.max = 1000)
str(km)
table(km$cluster)
kmclust = km$cluster

#Flexclust
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# Stocks clusters
stocksTrain1 = subset(stocksTrain, kmclust==1)
stocksTrain2 = subset(stocksTrain, kmclust==2)
stocksTrain3 = subset(stocksTrain, kmclust==3)

stocksTest1 = subset(stocksTest, kmclust==1)
stocksTest2 = subset(stocksTest, kmclust==2)
stocksTest3 = subset(stocksTest, kmclust==3)

# Compare mean dependent variable (PositiveDec)
summary(stocksTrain1)
summary(stocksTrain2)
summary(stocksTrain3)

# Models
StocksModel1 = glm(PositiveDec ~., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~., data=stocksTrain3, family=binomial)
summary(StocksModel1)

# Compare coefficients
StocksModel1$coeff
StocksModel2$coeff
StocksModel3$coeff

# Overall accuracies
PredictTest1 = predict(StocksModel1, type="response", newdata=stocksTest1)
table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
0.6194145

PredictTest2 = predict(StocksModel2, type="response", newdata=stocksTest2)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
0.5504808

PredictTest3 = predict(StocksModel3, type="response", newdata=stocksTest3)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
0.6458333

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
0.5788716