noisyFileName = "noisyTrainData.txt"

##noisyData
noisyDataDict = read.table(noisyFileName, sep = " ", fill = FALSE, strip.white = TRUE)

noisyDataDict = noisyDataDict[, 1:dimension]