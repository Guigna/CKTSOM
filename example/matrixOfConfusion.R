library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
##download data
#   http://homepage.tudelft.nl/n9d04/occ/

#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")

#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/504/oc_504.mat")
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/506/oc_506.mat")
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/517/oc_517.mat")

###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3

#set training and test
dataTraining <- data.frame(data$x$data[data$x$nlab == 2,])
dataTraining<-dataTraining[!duplicated(dataTraining), ] #elimina duplicados

dataTest <- data.frame(data$x$data)
dataTest<-dataTest[!duplicated(dataTest), ] #elimina duplicados

# Training with dataTraining
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, dataTraining)
# Calculus of mu and sigma
result <- calculateBmuDistance(neurons,dataTraining ,numberOfChildrenperNode,treeHeight)
mu <- mean(result[,2])
sigma <- sd(result[,2])
#clusterVisualization(dataTest,neurons,numberOfChildrenperNode) #plot the scatter plot


########
########
########
#howManyStandardDeviations<-0.5  # 1 (about 68%), 2 (about 95%) or 3 (about 99%)
vectorStandartDEsviation <- seq(0.1, 3, 0.1)
matrixOfConfusion<- calculateMatrixOfConfusion(neurons,dataTest,numberOfChildrenperNode,treeHeight,mu,sigma,dataTraining,vectorStandartDEsviation)


##########
################################    Fowlkes–Mallows index
###########
##vp = tp

FowlkesMallowsIndex <-sqrt((matrixOfConfusion$vp /(matrixOfConfusion$vp + matrixOfConfusion$fp) )* (matrixOfConfusion$vp /(matrixOfConfusion$vp + matrixOfConfusion$fn) ))
grafica<- data.frame(vectorStandartDEsviation,  FowlkesMallowsIndex)
#grafica$FowlkesMallowsIndex[is.nan(grafica$FowlkesMallowsIndex)]<-0
#ggplot(grafica, aes_string(x = "vectorStandartDEsviation", y = "FowlkesMallowsIndex"))+ geom_line()

###########
###################              exactitud   (vp+vn) / total
###########
#exactitud<- (matrixOfConfusion$vp + matrixOfConfusion$vn) / length(dataTest[,1])
#grafica2<- data.frame(vectorStandartDEsviation,  exactitud)
#####################################################################################





Grafico <-ggplot(grafica, aes_string(x = "vectorStandartDEsviation", y = "FowlkesMallowsIndex"))+ geom_line(size = 1.6,alpha= 0.5)


Grafico + xlab("Sigma") + ylab ("Fowlkes–Mallows index") +
  ggtitle("Test 4 \nDataset Diabetes") + # Título del gráficoa
  scale_y_continuous(breaks=seq(0, 1, 0.1),limit = c(0,1))+
  theme_bw()

