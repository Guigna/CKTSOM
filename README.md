# Self-Organizing Trees

Implemenation of the Self-Organizing Trees algorithm, a tree-based adaptation of the SOM.

#### DESCRIPTION

This R library has been designed for the construction and visualization of a k-ary Self-Organizing Trees.

In this implementation we consider complete k-ary trees that are defined using only two parameters, i.e, the depth of the tree and the number of children per node.

It is possible to train the tree using the Self-Organizing Maps (SOM) algorithm, but placed in the context of tree structures instead of the traditional grid structure. This requires an adaptation of the algorithm that implies a completely different behavior.

The SOM  is a machine learning algorithm that learns from data using a unsupervised learning paradigm, allowing the clustering of multidimensional data.

The Self Organizing Tree, present three main changes with respect to the SOM. First, it uses a tree structure instead of the traditional grid. Second, the neighborhood is defined through the hierarchical relationships of the trees up to the root, rather that the direct edges in the grid. Third the search for the fittest neuron, process known as the best matching unit search is performed on the hierarchical structure in log(n) time.

The graphical visualization is focused on a scatter plot that shows the tree connections and the weights as long as the data set being processed.

#### Instalation 

For installing the library you must execute the follwing command in R

```R
library(devtools)
install_github("Guigna/CKTSOM")
```

#### Example of execution 

##### Example 1

```R
library(CKTSOM)
library(ggplot2)

#set SEED
#setSeed(147)


##################### EXAMPLE 1 : IRIS DATASET
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
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
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
```
##### Example 3D: IRIS DATA
```R
library(CKTSOM)
library("rgl")

#set SEED
#setSeed(147)


##################### Example 3D: IRIS DATA
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
treeHeight <- 3

##training phase
data(iris)
data<-iris[-5] ## load a dataset
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)

##Grouping of neurons
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
plot3d(data$Sepal.Length,data$Sepal.Width,data$Petal.Length,size = 5,col= dataBMU,axes = FALSE,xlab ="",ylab ="",zlab ="")
points3d(neurons$Sepal.Length,neurons$Sepal.Width,neurons$Petal.Length,col=clusterVector,size = 7)
for (i in c(1:(length(neurons[,1])  - numberOfChildrenperNode ** treeHeight))) {
  mini <- miniLista(i,numberOfChildrenperNode)
  lines3d(neurons$Sepal.Length[mini],neurons$Sepal.Width[mini],neurons$Petal.Length[mini],size = 5,color="black")
}


##move plot
movie3d(spin3d(axis = c(0,0,1), rpm = 1), duration=120,  type = "png")
```
###### Result
![Demo](https://s17.postimg.org/40lj0d5q7/CKTSOM.gif)

##### Z-Score
```R
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
```

##### Example 2:  LIFECYLCE DATA
```R
library(CKTSOM)
library(ggplot2)

##################### EXAMPLE 2 : LIFECYLCE DATA
###parameters
numberOfIterations <- 600000
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 2
treeHeight <- 3

##training phase
data(LifeCycleSavings)
data<-LifeCycleSavings ## load a dataset
##remove outliers
data<-data[!(data$ddpi>10 | data$sr>20),]

ti <- proc.time() # start timer
neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
tf <-proc.time()    # stop timer
tf-ti #print execution time

##visualization phase
clusterVisualization(data,neurons,numberOfChildrenperNode) #plot the scatter plot
###########
#####################    visualization 2
###########
##Manual grouping of neurons
numberofGroups <- 4
clusterVector <- calculateGroups(numberofGroups,numberOfChildrenperNode,treeHeight)


##Calculate the group of each data
dataBMU<- calculateBMUForData(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight)

##visualization phase
##Display phase with grouping
clusterVisualization(data,neurons,numberOfChildrenperNode,clusterVector,dataBMU)
```
##### Valicdacion de one class usando AUC
Para esta validación se utiliza el indicador área bajo la curva ROC (AUC, área under the curve). Esto permite determinar qué tan bueno fue la evaluación detectando datos atípicos utilizando datos de prueba.

Los datos fueron obtenidos de la página del profesor David Tax, donde se encuentra una variedad de pruebas con distintos algoritmos proporcionando una tabla con la evaluación de cada uno de ellos, esto será utilizado para comparar y evaluar este algoritmo

Página del profesor David Taz: http://prlab.tudelft.nl/users/david-tax

Página de la biblioteca de datasets de one-class classification:  http://homepage.tudelft.nl/n9d04/occ/index.html


```R
require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)

## get data of
#  http://homepage.tudelft.nl/n9d04/occ/
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")

## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
labels <- labels -1
strata <- calculateStrata(labels)
trainSeting <- getDefaultTraingSeting()
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <-auc(dataT,labels,strata,standardDeviations,trainSeting,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

## Mean AUC
meanAuc <- c(1:length(out[,1]))
for (i in c(1:length(out[,1]))) {
  meanAuc[i]<- mean(out[i,])
}

##visualization
grafic<- data.frame(vectorStandartDesviation,  meanAuc)
plot <-ggplot(grafic, aes_string(x = "vectorStandartDesviation", y = "meanAuc"))+ geom_line(size = 1.6,alpha= 0.5)


plot + xlab("Sigma") + ylab ("Auc") +
  scale_y_continuous(breaks=seq(0, 1, 0.1),limit = c(0,1))
```
