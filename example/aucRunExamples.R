require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)
require("ROCR")
library(beepr)



###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 4)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out512a <- out
beep("mario")
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 4)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out512b <- out
beep("mario")
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 4,numberOfChildrenperNode = 4)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out512c <- out
beep("mario")
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 5,numberOfChildrenperNode = 4)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  print(n)
  out[n,] <- aucCalculate
  n<- n+1
}

out512d <- out
beep("mario")
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 4,numberOfChildrenperNode = 5)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  print(n)
  out[n,] <- aucCalculate
  n<- n+1
}

out512e <- out
beep("mario")
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 5,numberOfChildrenperNode = 5)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  print(n)
  out[n,] <- aucCalculate
  n<- n+1
}

out512f <- out
beep("mario")














###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/509/oc_509.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 5,numberOfChildrenperNode = 4,numberOfIterations=700000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  print(n)
  out[n,] <- aucCalculate
  n<- n+1
}

out509b <- out
beep("mario")
############
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/511/oc_511.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 5,numberOfChildrenperNode = 5,numberOfIterations=700000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509c <- out
beep("mario")



###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 6,numberOfChildrenperNode = 5,numberOfIterations=800000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509d <- out
beep("mario")


###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 6,numberOfChildrenperNode = 6,numberOfIterations=800000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509e <- out
beep("mario")

###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 7,numberOfChildrenperNode = 6,numberOfIterations=800000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509f <- out
beep("mario")

###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 7,numberOfChildrenperNode = 7,numberOfIterations=800000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509g <- out
beep("mario")

###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 8,numberOfChildrenperNode = 7,numberOfIterations=800000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509h <- out
beep("mario")

###########
#data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings(treeHeight = 8,numberOfChildrenperNode = 8,numberOfIterations=900000)
howManyAuc <- 5

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out509i <- out
beep("mario")






























###########################################################
out <- out514a


## Mean AUC
meanAuc <- c(1:length(out[,1]))
for (i in c(1:length(out[,1]))) {
  meanAuc[i]<- mean(out[i,])
}

##visualization
grafic<- data.frame(vectorStandartDesviation,  meanAuc)
plot <-ggplot(grafic, aes_string(x = "vectorStandartDesviation", y = "meanAuc"))+ geom_line(size = 1.6,alpha= 0.5)


plot + xlab("Sigma") + ylab ("AUC") +
  scale_y_continuous(breaks=seq(0, 1, 0.1),limit = c(0,1))


maxAUC <-max(meanAuc)
maxAUC * 100
matc <- match(maxAUC,meanAuc)
vectorStandartDesviation[matc]

write.csv(out, file = "514  Arrhythmia normal h=4.csv")




##
#COmprar los graficos para ver patrones en la capacidad de domprimir los datos
# usar el primer grafico a la derecha

##Jugar con la configuracion del arbol para hacerlo trabajar con lso datos donde se encontro mejor resultado
# probar con los otros data si afecta el usar un arbol diferente (mas grande)


##PCA

#######3Imagen para el rank
rank$Promedio

ggplot(data=rank, aes(x=Nombre, y=Promedio)) + geom_bar(stat="identity")


ggplot(data=rank, aes(x=reorder(Nombre,+Promedio), y=Promedio, fill=reorder(Nombre,+Promedio))) +
  geom_bar(stat="identity") + xlab("") + ylab ("") + scale_x_discrete(NULL) + theme(axis.title.x=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank())+
  guides(fill=guide_legend(title="Classifiers "))
