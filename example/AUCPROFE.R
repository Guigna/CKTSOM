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
calculateRMSE(data,trainSettings,10,4,5,seed = 543)


trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 3,treeHeight = 3,
                                          initialLearningRate =1 ,finalLearningRate = 0.0,
                                          initialRadius = 3,finalRadius = 0,numberOfIterations=1000)

calculateAUC(data,labels,trainSettings,seed = 543)


