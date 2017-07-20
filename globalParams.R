filePath = "/home/shatroopa/Desktop/cHEMICAL lOCHA/imp/brainModelling/kMeans"

k = 2                         ##number of clusters

fileName = "state_0.csv"
pureDataDict = read.table(fileName, sep = ",", fill = FALSE, strip.white = TRUE)

nPoints = nrow(pureDataDict)         ##number of points in the data dictionary
dimension = ncol(pureDataDict) - 1   ##dimension of each data point

##separate the clustering vector from data
expectedClusteringVector = pureDataDict[[dimension + 1]]
expectedClusteringVector = expectedClusteringVector + 1
pureDataDict = pureDataDict[, 1:dimension]


##a point is a row of the dataDict
retreivePoint <- function(dataDict, i){
  return(as.numeric(dataDict[i,]))
}

countTrue <- function(givenVector)
{
  count = 0
  for(i in 1:length(givenVector))
  {
    if(givenVector[i] == T)
      count = count + 1
  }
  return(count)
}

## a data frame: every row is a point vector
activeCount = 0
for(i in 1:nPoints)
{

  if(expectedClusteringVector[i] == 2)
  {
    activeCount = activeCount + 1
  }
 
}

print(activeCount)