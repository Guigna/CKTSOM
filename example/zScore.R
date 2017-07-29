library(CKTSOM)
library(ggplot2)
##################### IRIS DATASET
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3

data(iris)
data<-iris[-5] ## load a dataset
##Execution algorithm
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)

########
########     Outliers
########
howManyStandardDeviations <- 1   # 1 (about 68%), 2 (about 95%) or 3 (about 99%)
outliers <- getOutliers(neurons,data ,numberOfChildrenperNode,treeHeight,howManyStandardDeviations)

#delete outlayer of data
procesData <- data[-outliers,]

##Grouping of neurons
numberofGroups <- 3
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)

##Calculate the group of each data
dataBMU<- calculateBMUForData(procesData,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(procesData,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
