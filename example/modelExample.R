library(CKTSOM)
library(ggplot2)
library(R.matlab)


require(sampling)
#install.packages("R.matlab")
library(dplyr)
require("ROCR")
library(beepr)


trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 3,treeHeight = 2,
                                          initialLearningRate =1 ,finalLearningRate = 0.0,
                                          initialRadius = 3,finalRadius = 0)

dataMat <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")
data <-data.frame(dataMat$x$data)
labels <- dataMat$x$nlab


strataConfig <- calculateStrata(labels,0.5)


dataTraining <- data[labels ==2,]
idTrain<-sample( 1:nrow( dataTraining ), strataConfig )
dataTraining <- dataTraining[idTrain,]

idTrain <- as.numeric(row.names(dataTraining))



#Generar modelo
model <- buildCKTSOM(dataTraining,trainSettings)



dataTest <- data[-idTrain,]
resultadoEsoperado <- labels[-idTrain]

###Prediccion
model$prediction(dataTest,threshold =0.1)


###PGrafica
clusterVisualizationZone(dataTest,model,threshold = 0.1)
############  + coord_fixed()

