
# Load the data
airlines <- read.csv("./AirlinesCluster.csv")
str(airlines)

summary(airlines)

# Normalize the data
install.packages("caret")
library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
sd(airlinesNorm)

# Compute distances and hierarchical clustering
airlinesDist = dist(airlinesNorm, method = "euclidean")

# Select 5 clusters and Number of observations in each cluster
clusterGroups = cutree(ClusterIntensity, k = 5)
table(clusterGroups)

# Compute the average values per cluster using tapply()
tapply(Households$NumVisits, kmeansClust, mean)
tapply(Households$AvgProdCount, kmeansClust, mean)
tapply(Households$AvgDiscount, kmeansClust, mean)
tapply(Households$AvgSalesValue, kmeansClust, mean)
tapply(Households$MorningPct, kmeansClust, mean)
tapply(Households$AfternoonPct, kmeansClust, mean)

# K-mean clustering
k=5
set.seed(88)
kmeans = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(kmeans)
table(kmeans$cluster)

kmeansClust = kmeans$cluster
kmeansClust

# Compare clusters

kmcluster1 = subset(airlinesNorm, kmeansClust==1)
kmcluster2 = subset(airlinesNorm, kmeansClust==2)
kmcluster3 = subset(airlinesNorm, kmeansClust==3)
kmcluster4 = subset(airlinesNorm, kmeansClust==4)
kmcluster5 = subset(airlinesNorm, kmeansClust==5)

# Clusters