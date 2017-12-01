require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)
require("ROCR")
library(beepr)

dataMat <- readMat("http://homepage.tudelft.nl/n9d04/occ/511/oc_511.mat")
data <-data.frame(dataMat$x$data)
labels <- dataMat$x$nlab

trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 3,treeHeight = 3,
                                        initialLearningRate =1 ,finalLearningRate = 0.0,
                                        initialRadius = 3,finalRadius = 0)

calculateRMSE(data,trainSettings,log= c(10,20,30,50),seed = 543)



x<-calculateAUC(data,labels,
                numberOfChildrenperNodeList=c(2),
                treeHeightList = c(3,4),
                initialLearningRateList = c(1,0.8,0.5),
                finalLearningRateList = c(0.0),
                initialRadiusList = c(3,6),
                finalRadiusList = c(0),
                numberOfIterationsList= c(1000,10000),
                seed = 543)


