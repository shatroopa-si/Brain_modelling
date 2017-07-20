euclidDistance <- function(point1, point2)
{
  ##returns the euclidean distance between 2 points(taken as a vector)
  
  total = 0                                 ##accumulator variable
  
  for(index in 1:dimension){
    diff = (point1[index] - point2[index]) ** 2
    total = total + diff
  }
  
  distance = sqrt(total)
  
  return(distance)
}


initCentroid <- function(dataDict, k)
{
  ##randomly chosen data points will form the centroid
  
  centroids = list()                #list of points(vector)
  
  #generate k unique random integers between 1 & nPoints which will be the index of chosen centroid in dataDict
  cKeys = sample(1:nPoints, k, replace = F)

  centroidCount = 1
  while(centroidCount <= k)
  {
    centroids[[centroidCount]] = retreivePoint(dataDict, cKeys[centroidCount])
    centroidCount = centroidCount + 1
  }
  
  return(centroids)
}


createClusters <- function(dataDict, k, iterations, centroids, clusteringVector)
{
  ##creates k clusters using initial set of centroids, the data dictionary, applying 'iterations' number of repeats
  # print("::::::Initially chosen centroids:::::")
  # print(centroids)
  for(iter in 1:iterations)
  {
    #print('*********************PASS**********************')
    
    ##initialize k clusters
    clusters = list()                   #list of matrix (each row is a point in the cluster)
    for(i in 1:k)
    {
      clusters[[i]] = matrix(nrow = 0, ncol = dimension)
    }
    
    ##assign each data point to a cluster corresponding to the centroid it is closest to
    for(i in 1:nPoints)
    {
      point = retreivePoint(dataDict, i)
      
      ##calculate distance from each cluster and find the minimum
      dist = vector(length = k)
      for(j in 1:k)
      {
        dist[j] = euclidDistance(point, centroids[[j]])
      }
      minDist = min(dist)
      
      ##assign the point to the particular cluster
      x = which(minDist == dist)[[1]]               #index of the cluster with minimum distance from the point
      clusters[[x]] = rbind(clusters[[x]], point)
      clusteringVector[i] <<- x                     #assignment takes place in the global environment
    }
    
    ##recompute centroids(median method for multi-dimensional points) for each of the k clusters 
    for(i in 1:k)
    {
      sums = colSums(clusters[[i]])/dim(clusters[[i]])[1]
      centroids[[i]] <- sums
    }
    
    ##repopulate empty clusters by choosing a random centroid
    for(i in 1:k)
    {
      if(dim(clusters[[i]])[1] == 0)                #no rows in the matrix ====>> empty cluster
      {
        centroids[[i]] <- retreivePoint(dataDict, sample(1:nPoints, 1, replace = F))
      }
    }
  }
  #print(centroids)
  #return(clusters)
  return(centroids)
}