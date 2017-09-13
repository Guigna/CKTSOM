require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)
require("ROCR")

## get data of
#  http://homepage.tudelft.nl/n9d04/occ/

###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/547/oc_547.mat")
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

out547 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/548/oc_548.mat")
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

out548 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/549/oc_549.mat")
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

out549 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/550/oc_550.mat")
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

out550 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/551/oc_551.mat")
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

out551 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/552/oc_552.mat")
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

out552 <- out
###############
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/553/oc_553.mat")
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

out553 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/554/oc_554.mat")
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

out554 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/555/oc_555.mat")
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

out555 <- out





###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/556/oc_556.mat")
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

out556 <- out
######################
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/557/oc_557.mat")
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

out557 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/570/oc_570.mat")
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

out570 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/571/oc_571.mat")
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

out571 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/572/oc_572.mat")
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

out572 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/573/oc_573.mat")
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

out573 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/574/oc_574.mat")
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

out574 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/575/oc_575.mat")
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

out575 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/576/oc_576.mat")
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

out576 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/577/oc_577.mat")
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

out577 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/578/oc_578.mat")
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

out578 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/579/oc_579.mat")
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

out579 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/580/oc_580.mat")
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

out580 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/581/oc_581.mat")
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

out581 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/582/oc_582.mat")
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

out582 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/583/oc_583.mat")
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

out583 <- out
###########
data <- readMat("http://homepage.tudelft.nl/n9d04/occ/584/oc_584.mat")
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

out584 <- out




###########################################################
out <- out546

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

write.csv(out, file = "546  Delft pump 1x3.csv")
