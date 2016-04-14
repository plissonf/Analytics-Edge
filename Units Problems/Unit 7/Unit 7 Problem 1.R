# Set working directory
setwd("./Desktop/Analytics Edge/Unit 7/")

# Load libraries
library(ggplot2)
library(maps)
library(ggmap)

# Load US map
statesMap = map_data("state")
str(statesMap)

# Draw map
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")


# Load imputed polling data 
polling = read.csv("./PollingImputed.csv")
str(polling)

# Subset data into training set and test set (Copy recitation Unit 3)
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Logistic regression model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
summary(mod2)

# Training set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPrediction # predicted Republican/Democrat probabilities for each state 

# Vector for Predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# Merge predictions and labels into dataframe based on state
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)
table(predictionDataFrame$TestPredictionBinary==1)
summary(predictionDataFrame)


# Merge predictionDataFrame with statesMap
# lowercase states
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

# Color map
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

which(predictionDataFrame$Test.State=="Florida")
predictionDataFrame$TestPrediction[6]

# ?geom_polygon()
