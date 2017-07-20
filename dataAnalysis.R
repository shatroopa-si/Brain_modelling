clusteringVector = vector(length = nPoints)     # which data point was allotted which cluster

## analyzing pure data
analysis <- function(dataset)
{
  mapCluster = vector(length = k)
  
  centroids = initCentroid(dataset, k)
  #clusters = createClusters(dataset, k, 50, centroids, clusteringVector)
  centroids = createClusters(dataset, k, 50, centroids, clusteringVector)
  
  # print("::::::Final centroids:::::")
  # print(centroids)
  
  ##create a map in cluster numbers and apply the map on clustering vector
  mapCluster[clusteringVector[1]] = expectedClusteringVector[1]
  mapCluster[3 - clusteringVector[1]] = 3 - expectedClusteringVector[1]
  for(i in 1:nPoints)
  {
    clusteringVector[i] = mapCluster[clusteringVector[i]]
  }
  
  
  ##compare
  matches = countTrue(clusteringVector == expectedClusteringVector)
  cat("Matches = ", matches)
  cat("\n")
  efficiency = (matches / nPoints * 1.0) * 100
  cat("Efficiency = ", efficiency)
  cat("\n")
  
  # ##silhouette plot with built-in k
  # library(cluster)
  # silhouettePlot = silhouette(dataset, dist(dataset))
  # plot(silhouettePlot, col = 1:k)
  
  return(centroids)
}


## analyze pure data------------------------------------------------------------
print(":::::::::::::::::Pure data analysis:::::::::::::::::")
centroidsPureData = analysis(pureDataDict)

##test--
#load file
testFileName = "testData.txt"
testDataDict = read.table(testFileName, sep = " ", fill = FALSE, strip.white = TRUE)
nTestPoints = nrow(testDataDict)
expectedTestClusteringVector = testDataDict[[dimension + 1]]
expectedTestClusteringVector = expectedTestClusteringVector + 1
testDataDict = testDataDict[, 1:dimension]

##assign clusters to each point
testClusteringVector = vector(length = nTestPoints)
for(i in 1:nTestPoints)
{
  point = retreivePoint(testDataDict, i)
  
  ##calculate distance from each cluster and find the minimum
  dist = vector(length = k)
  for(j in 1:k)
  {
    dist[j] = euclidDistance(point, centroidsPureData[[j]])
  }
  minDist = min(dist)
  
  ##assign the point to the particular cluster
  x = which(minDist == dist)[[1]]               #index of the cluster with minimum distance from the point
  #clusters[[x]] = rbind(clusters[[x]], point)
  testClusteringVector[i] <- x                     #assignment takes place in the global environment
}

##check
mapCluster = vector(length = k)
mapCluster[testClusteringVector[1]] = expectedTestClusteringVector[1]
mapCluster[3 - testClusteringVector[1]] = 3 - expectedTestClusteringVector[1]
for(i in 1:nTestPoints)
{
  testClusteringVector[i] = mapCluster[testClusteringVector[i]]
}
matches = countTrue(testClusteringVector == expectedTestClusteringVector)
cat("Matches = ", matches)
cat("\n")
efficiency = (matches / nTestPoints * 1.0) * 100
cat("Efficiency = ", efficiency)
cat("\n")


## analyze noisy data-----------------------------------------------------------
print(":::::::::::::::::Noisy data analysis:::::::::::::::::")
centroidsNoisyData = analysis(noisyDataDict)

##test--
##assign clusters to each point
testClusteringVector = vector(length = nTestPoints)
for(i in 1:nTestPoints)
{
  point = retreivePoint(testDataDict, i)
  
  ##calculate distance from each cluster and find the minimum
  dist = vector(length = k)
  for(j in 1:k)
  {
    dist[j] = euclidDistance(point, centroidsNoisyData[[j]])
  }
  minDist = min(dist)
  
  ##assign the point to the particular cluster
  x = which(minDist == dist)[[1]]               #index of the cluster with minimum distance from the point
  #clusters[[x]] = rbind(clusters[[x]], point)
  testClusteringVector[i] <- x                     #assignment takes place in the global environment
}

##check
mapCluster = vector(length = k)
mapCluster[testClusteringVector[1]] = expectedTestClusteringVector[1]
mapCluster[3 - testClusteringVector[1]] = 3 - expectedTestClusteringVector[1]
for(i in 1:nTestPoints)
{
  testClusteringVector[i] = mapCluster[testClusteringVector[i]]
}
matches = countTrue(testClusteringVector == expectedTestClusteringVector)
cat("Matches = ", matches)
cat("\n")
efficiency = (matches / nTestPoints * 1.0) * 100
cat("Efficiency = ", efficiency)
cat("\n")

##introduce gaussian noise==========increase noise power
