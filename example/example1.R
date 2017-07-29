library(CKTSOM)
library(ggplot2)

#set SEED
setSeed(543)


##################### EXAMPLE 1 : IRIS DATASET
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3

##training phase
data(iris)
data<-iris[-5] ## load a dataset
ti <- proc.time() # start timer
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop timer
tf-ti #print execution time

##visualization phase
##Display phase without grouping
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot
###########
#####################    visualization 2
###########

##Grouping of neurons
numberofGroups <- 3
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
