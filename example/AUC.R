#############################################################
################        AUC exaple          #################
#############################################################

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
maxAUC * 100  # best AUC
matc <- match(maxAUC,meanAuc)
vectorStandartDesviation[matc]  # theta for best AUC
