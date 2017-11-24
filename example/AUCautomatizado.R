
require(sampling)
library(ggplot2)
#install.packages("R.matlab")
library(R.matlab)
library(CKTSOM)
library(dplyr)
require("ROCR")
library(beepr)

#install_github("Guigna/CKTSOM")

#Carga los datos
dataMat <- readMat("http://homepage.tudelft.nl/n9d04/occ/556/oc_556.mat")
################################################          1           ################################
###########   obtiene el RMSE para el dato cargado, entregando un rango y la iteracion
################################################          1           ################################
## set parameters
data <-data.frame(dataMat$x$data)


totalPruebas<- 9
iniLearn <- 0.8
hijosPerNode<- 4
iteraciones <- 10000

trainignSetingMatrix <- matrix(ncol = 7,nrow = totalPruebas)


trainignSetingMatrix[1,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 2,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 4,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[2,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 2,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 2,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[3,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 2,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 1,finalRadius = 0,
                                                numberOfIterations = iteraciones)

trainignSetingMatrix[4,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 3,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 6,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[5,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 3,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 3,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[6,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 3,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 2,finalRadius = 0,
                                                numberOfIterations = iteraciones)

trainignSetingMatrix[7,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 4,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 8,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[8,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 4,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 4,finalRadius = 0,
                                                numberOfIterations = iteraciones)
trainignSetingMatrix[9,]<- getDefaultTraingSettings(numberOfChildrenperNode = hijosPerNode,treeHeight = 4,
                                                initialLearningRate =iniLearn ,finalLearningRate = 0.0,
                                                initialRadius = 2,finalRadius = 0,
                                                numberOfIterations = iteraciones)




labels <- dataMat$x$nlab
strata <- calculateStrata(labels,0.5)

howManyAuc <- 20 # cuantas veces se hace el experimento
vectorStandartDesviation <- c(0.1,seq(0.5, 3, 0.5))  ## se genera una secuencia de distinos valores para el z-score   (0.1 0.5 1.0 1.5 2.0 2.5 3.0)

resultados <-matrix(ncol = 10,nrow = totalPruebas)
#iterar
for(prueba in 1:totalPruebas){
  trainSettings <- trainignSetingMatrix[prueba,]


  out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
  n<- 1
  for (standardDeviations in vectorStandartDesviation) {
    setSeed(543)
    aucCalculate <- validate(data,labels,strata,standardDeviations,trainSettings,howManyAuc) ## una ejecucion
    print(n)
    out[n,] <- aucCalculate
    n<- n+1
  }
  #beep("mario")
  ################################################          4           ################################
  ###########   Muestra las estadisticas del AUC
  ################################################          4           ################################

  ## Mean AUC
  meanAuc <- c(1:length(out[,1]))
  for (i in c(1:length(out[,1]))) {
    meanAuc[i]<- mean(out[i,])
  }

  ##visualization
  #grafic<- data.frame(vectorStandartDesviation,  meanAuc)
  #plot <-ggplot(grafic, aes_string(x = "vectorStandartDesviation", y = "meanAuc"))+ geom_line(size = 1.6,alpha= 0.5)


  #plot + xlab("Sigma") + ylab ("AUC") +
   # scale_y_continuous(breaks=seq(0, 1, 0.1),limit = c(0,1))

  maxAUC <-max(meanAuc)
  maxAUC * 100  ##AUC maximo
  matc <- match(maxAUC,meanAuc)
  sd(out[matc,])*100  # desviacion estandar
  vectorStandartDesviation[matc] # Theta usado en el Z-score
  c(trainSettings)
  c(maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc])

  resultados[prueba,]<-c(trainSettings,maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc])

}
resultados
beep("mario")
