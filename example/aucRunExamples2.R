require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)
require("ROCR")
library(beepr)



###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.5)
trainSettings <- getDefaultTraingSettings()
howManyAuc <- 20

## calculate AUC
vectorStandartDesviation <- seq(0.1, 3, 0.1)
out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
n<- 1
for (standardDeviations in vectorStandartDesviation) {
  aucCalculate <- validate5x10cv(dataT,labels,strata,standardDeviations,trainSettings,howManyAuc)
  out[n,] <- aucCalculate
  n<- n+1
}

out501 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/502/oc_502.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out502 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/503/oc_503.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out503 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/504/oc_504.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out504 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/505/oc_505.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out505 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/506/oc_506.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out506 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/507/oc_507.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out507 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/508/oc_508.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out508 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/509/oc_509.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out509 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/511/oc_511.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out511 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/512/oc_512.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out512 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/513/oc_513.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out513 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/514/oc_514.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out514 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/515/oc_515.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out515 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/516/oc_516.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out516 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/517/oc_517.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out517 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/518/oc_518.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out518 <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/519/oc_519.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out519b <- out
###########################
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/520/oc_520.mat")
## set parameters
dataT <-data.frame(data$x$data)
labels <- data$x$nlab
strata <- calculateStrata(labels,0.9)
trainSettings <- getDefaultTraingSettings()
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

out520 <- out
###########################
out501[1,3] <- mean(c(out501[1,1],out501[1,2],out501[1,3],out501[1,4]))

out518[29,5] <- mean(c(out518[29,1],out518[29,2]))
out518[30,4] <- mean(c(out518[30,1],out518[30,2],out518[30,3]))

###########################################################
out <- out501
out




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

write.csv(out, file = "520 Concordia16 digit 0")

