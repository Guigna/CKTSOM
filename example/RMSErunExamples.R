library(CKTSOM)
library(ggplot2)
library(R.matlab)
library(beepr)


require(sampling)
#install.packages("R.matlab")
library(dplyr)
require("ROCR")




initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3

##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE01 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/511/oc_511.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE11 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE12 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE14 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/517/oc_517.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE17 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/519/oc_519.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE19 <- RMSE
##########3
#############
#######
###
##training phase
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/520/oc_520.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 11
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE20 <- RMSE
##########3
#############
#######
###


RMSE <-  RMSE11

Iteration <- c(1:total)
grafico <- data.frame(Iteration,RMSE)
ggplot(grafico,aes(x = Iteration,y =RMSE)) + geom_line(size=1.5,alpha=0.5) +
  scale_x_continuous(breaks =  Iteration ,labels=iteraciones) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

#

data <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")
## set parameters
dataT <-data.frame(data$x$data)


total <- 9
iteraciones <- c(1:total)
RMSE <- c(1:total)
itera <- 10

for ( i in iteraciones){
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,itera, dataT)
  out <- matrix(, nrow = length(dataT[,1]), ncol = 2)
  datosa <- c(1:length(dataT[,1]))
  for(j in datosa){
    out[j,] <- findBmuAndDistance(neurons,dataT[j,],numberOfChildrenperNode,treeHeight)
  }
  RMSE[i]<- sqrt(mean(out[,2]))  ### el out[2] esta elevado
  iteraciones[i] <- itera
  itera <- itera * 4
}
beep("mario")
RMSE11 <- RMSE



RMSE <-  RMSE11

Iteration <- c(1:total)
grafico <- data.frame(Iteration,RMSE)
ggplot(grafico,aes(x = Iteration,y =RMSE)) + geom_line(size=1.5,alpha=0.5) +
  scale_x_continuous(breaks =  Iteration ,labels=iteraciones) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

#
