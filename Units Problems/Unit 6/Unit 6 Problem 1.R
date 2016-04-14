setwd("./Desktop/Analytics Edge/Unit 6/")

# Load the data
dailykos <- read.csv("./dailykos.csv")
str(dailykos)

## No need to compute matrix and vector for that exercise
# Change the data type to matrix
dailykosMatrix <- as.matrix(dailykos)
str(dailykosMatrix)

# Change the matrix to vector
dailykosVector <- as.vector(dailykosMatrix)
str(dailykosVector)

# Compute distances
distance = dist(dailykos, method = "euclidean")

# Hierarchical clustering
ClusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(ClusterIntensity)

# Select 7 clusters
rect.hclust(ClusterIntensity, k = 7, border = "red")
dailykosHClusters = cutree(ClusterIntensity, k = 7)
dailykosHClusters

# Number of observations in each cluster
table(dailykosHClusters)

#Define cluster 1
hcluster1 = subset(dailykos, dailykosHClusters==1)
str(hcluster1)

# Compute frequencies in words of cluster 1 and output 6 top words
tail(sort(colMeans(hcluster1)))


# For cluster 2
hcluster2 = subset(dailykos, dailykosHClusters==2)
str(hcluster2)
tail(sort(colMeans(hcluster2)))

# Find which cluster Iraq war is in
#solution 1: create other clusters and apply tail(sort(colMeans(clusterX))) until the word "iraq" appears in the top 6
hcluster3 = subset(dailykos, dailykosHClusters==3)
hcluster4 = subset(dailykos, dailykosHClusters==4)
hcluster5 = subset(dailykos, dailykosHClusters==5)
hcluster6 = subset(dailykos, dailykosHClusters==6)
hcluster7 = subset(dailykos, dailykosHClusters==7)
tail(sort(colMeans(hcluster3)))
tail(sort(colMeans(hcluster4)))
tail(sort(colMeans(hcluster5)))
tail(sort(colMeans(hcluster6)))
tail(sort(colMeans(hcluster7)))

#solution 2: identify which line the observation "iraq" is and find associated cluster
subset(dailykos, word=="iraq")
dailykosHClusters[722]

#solution 3: compute mean for observation and associated cluster
tapply(dailykos$iraq, dailykosHClusters, mean)

# K-means Clustering
k=7
set.seed(1000)
KMC = kmeans(dailykos, centers = k)
str(KMC)

# Extract clusters
dailykosClusters <- KMC$cluster
table(dailykosClusters)

kmcluster1 = subset(dailykos, dailykosClusters==1)
kmcluster2 = subset(dailykos, dailykosClusters==2)
kmcluster3 = subset(dailykos, dailykosClusters==3)
kmcluster4 = subset(dailykos, dailykosClusters==4)
kmcluster5 = subset(dailykos, dailykosClusters==5)
kmcluster6 = subset(dailykos, dailykosClusters==6)
kmcluster7 = subset(dailykos, dailykosClusters==7)

# Compare Hierachical and K-means clustering - cluster 2
table(kmcluster2)
