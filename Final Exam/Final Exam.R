## PROBLEM 1 - Predicting Box Office Revenue

# Working directory
getwd()
setwd("./Desktop/Analytics Edge/Final Exam/")

# Loading the data
Movies <- read.csv("./Movies.csv")
MoviesTrain <- subset(Movies, Year<2010)
MoviesTest <- subset(Movies, Year>=2010)

# Linear regression model
ModelLR = lm(Worldwide~., data=MoviesTrain[ , 3:ncol(MoviesTrain)])
summary(ModelLR)

# Correlation
cor(MoviesTrain[ , 3:ncol(MoviesTrain)])

# 2nd Linear Regression Model  
ModelLR2 = lm(Worldwide~ Runtime + Crime + Horror + Animation + History + Nominations + Production.Budget, data=MoviesTrain[ , 3:ncol(MoviesTrain)])

# Predictions
ModelLR2.pred = predict(ModelLR2, newdata=MoviesTest)
ModelLR2.sse = sum((ModelLR2.pred - MoviesTest$Worldwide)^2)
ModelLR2.sse

ModelLR2.sst = sum((mean(MoviesTest$Worldwide) - MoviesTest$Worldwide)^2)
ModelLR2.sst

R2 = 1 - (ModelLR2.sse/ModelLR2.sst)
R2

# New dependent variable "Performance" takes three different values: "Excellent", "Average"
Movies$Performance = factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent", ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average", "Poor")))

# Remove dependent variable "Worldwide"
Movies$Worldwide = NULL

# Split sample
library(caTools)
set.seed(15071)
spl = sample.split(Movies$Performance, SplitRatio = 0.7)
MoviesTrain = subset(Movies, spl==TRUE)
MoviesTest = subset(Movies, spl==FALSE)

# CART model
library(rpart)
library(rpart.plot)

MoviesTree = rpart(Performance ~. , data=MoviesTrain[ , 3:ncol(MoviesTrain)], method="class")
prp(MoviesTree)

# Confusion matrices and baseline model
PredictTrain = predict(MoviesTree, type="class")
table(MoviesTrain$Performance, PredictTrain)
(96+41+46)/nrow(MoviesTrain) 0.7820513

table(MoviesTrain$Performance) 0.4957265

PredictTest = predict(MoviesTree, newdata=MoviesTest, type="class")
table(MoviesTest$Performance, PredictTest)
(36+16+16)/nrow(MoviesTest)  0.68

table(MoviesTest$Performance) 0.5

## PROBLEM 2 - FORECASTING INTEREST RATE HIKES BY THE US FEDERAL RESERVE
#Loading the data
fedFunds <- read.csv("./federalFundsRate.csv", stringsAsFactors=FALSE)

# Proportion - increased rate
table(fedFunds$Streak>0) 0.5042735

# Longest Chairman
table(fedFunds$Chairman)

# Convert into factors
fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

# Split data
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training = subset(fedFunds, spl==TRUE)
testing = subset(fedFunds, spl==FALSE)

# Logistic regression model
Mod = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, family=binomial)
summary(Mod)

9.121012 - 0.003427*1.7 - 0.157658*3 - 0.047449*5.1 - 0.136451*65.3 + 0.347829*0 - 0.006931*18
= -0.6347861 or 0.6347861 why false?

exp(0.347829) 1.41599

# Prediction and confusion matrix
PredTest = predict(Mod, newdata=testing, type="response")
summary(PredTest)
table(testing$RaisedFedFunds, PredTest>0.5)

Baseline Model accuracy
table(testing$RaisedFedFunds)

table(PredTest>0.5)

# AUC + ROC plot
install.packages("ROCR")
library(ROCR)
predROCR = prediction(PredTest, testing$RaisedFedFunds)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

# Cross-validation CART model
set.seed(201)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl(method = "cv", number = 10) # 10-fold cross validation
cpGrid = expand.grid(.cp = seq(0.001,0.050,0.001)) # 50 values of +0.001 (0.001-0.05)

train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
treecv = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, cp=0.23)
prp(treecv)

training.pred = predict(treecv)
training$RaisedFedFunds = as.factor(training$RaisedFedFunds)
testing.pred = predict(treecv, newdata=testing, type="class")
testing.pred
table(testing$RaisedFedFunds, testing.pred)

## PROBLEM 3 - Understanding retail consumers
# Loading data
Households = read.csv("./Households.csv")

# 1
table(Households$MorningPct>=100)
table(Households$AfternoonPct>=100)

# 2
sub = subset(Households, Households$AvgSalesValue>150)
which.min(sub$AvgDiscount)

sub2 = subset(Households, Households$AvgDiscount>25)
which.min(sub2$AvgSalesValue)

sub3 = subset(Households, Households$NumVisits>=300)
str(sub3) 148/2500=0.0592

# 4 - Normalise Data
library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)
summary(HouseholdsNorm)

# Dendrogram
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

2, 3 and 5 clusters

#K-means clustering
k=5
set.seed(500)
kmeans = kmeans(HouseholdsNorm, centers = k, iter.max = 1000)

kmeans$cluster
table(kmeans$cluster)
kmeansClust = kmeans$cluster
kmeansClust

#Compare clusters
kmcluster1 = subset(HouseholdsNorm, kmeansClust==1)
kmcluster2 = subset(HouseholdsNorm, kmeansClust==2)
kmcluster3 = subset(HouseholdsNorm, kmeansClust==3)
kmcluster4 = subset(HouseholdsNorm, kmeansClust==4)
kmcluster5 = subset(HouseholdsNorm, kmeansClust==5)
kmcluster6 = subset(HouseholdsNorm, kmeansClust==6)
kmcluster7 = subset(HouseholdsNorm, kmeansClust==7)
kmcluster8 = subset(HouseholdsNorm, kmeansClust==8)
kmcluster9 = subset(HouseholdsNorm, kmeansClust==9)
kmcluster10 = subset(HouseholdsNorm, kmeansClust==10)

#
tapply(Households$NumVisits, kmeansClust, mean)
tapply(Households$AvgProdCount, kmeansClust, mean)
tapply(Households$AvgDiscount, kmeansClust, mean)
tapply(Households$AvgSalesValue, kmeansClust, mean)
tapply(Households$MorningPct, kmeansClust, mean)
tapply(Households$AfternoonPct, kmeansClust, mean)