library(CKTSOM)
library(ggplot2)
library(R.matlab)
#set SEED



##################### EXAMPLE 1 : IRIS DATASET
###parameters
numberOfIterations <- 1
initialLearningRate <- 1
finalLearningRate<- 0
initialRadius <- 7
finalRadius <- 1
numberOfChildrenperNode <- 3
treeHeight <- 3

##training phase


dataMAT <- readMat("http://homepage.tudelft.nl/n9d04/occ/570/oc_570.mat")
## set parameters
data <-data.frame(dataMAT$x$data)

#######
#length(data[,1])


#######
ciclos <- 5
list <- c(0:ciclos)
salidas <- c(1:ciclos+1)
itera <- c(1:ciclos+1)

####
vectorTrreFindMatrix<-  matrix(nrow = length(data[,1]),ncol = ciclos+1)
vectorHardFindMatrix<- matrix(nrow = length(data[,1]),ncol = ciclos+1)
###



for(lis in list){

  numberOfIterations <- 10**(lis)
  itera[lis+1] <- numberOfIterations
  setSeed(543)
  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)

  vectorTrreFind <- c(1:length(data[,1]))
  vectorHardFind <- c(1:length(data[,1]))

  for (i in vectorTrreFind) {
    #busqueda en profundidad
    vectorTrreFind[i] <- findBmuAndDistance( neurons,data[i,], numberOfChildrenperNode, treeHeight)[1]
    #busqueda exhaustiva
    vectorHardFind[i] <- hardFindBmu(neurons,data[i,], numberOfChildrenperNode, treeHeight)
  }
  ##
  vectorTrreFindMatrix[,lis+1]<- vectorTrreFind
  vectorHardFindMatrix[,lis+1]<- vectorHardFind
  ##
  sum <- 0
  i<-1
  while (i <= length(data[,1]) ){
    if(vectorTrreFind[i] == vectorHardFind[i]){
      sum <- sum +1
    }
    i <- i + 1
  }

  salidas[lis+1]<-sum
  if(lis == 0){
    totalNeurona<- data.frame(neurons)
  } else {
    totalNeurona<- data.frame(totalNeurona,neurons)
  }
}



calculo <- c(1:length(salidas))

for( i in calculo){
  calculo[i] <-    salidas[i]/length(data[,1])
}
calculo ## porcentaje de veces que se encuentra la verdadera BMU



#####
##########  Distancia promedio hasta la BMU real  (normalizando las neuronas entre 0 y 1)
####


dimencionNeuron <- length(neurons[1,])
iteracionVectorr<-c(1:6)
for(iteracion in iteracionVectorr){
  iterationNeuronTreeIndex <- c(((iteracion-1)*dimencionNeuron+1):(iteracion*dimencionNeuron))
  neuronTemporal <- normalizeDataFRame(totalNeurona[,iterationNeuronTreeIndex])
  neuronTemporal[is.na(neuronTemporal)] <- 0
  distancia<- c(1:length(data[,1]))
  for(i in distancia){
    distancia[i] <- sum(sqrt((neuronTemporal[vectorHardFindMatrix[i,iteracion],] -   neuronTemporal[vectorTrreFindMatrix[i,iteracion],])**2))

  }
  iteracionVectorr[iteracion]<- mean(distancia)
}
iteracionVectorr  ## distancia promedio hasta hasta el BMU real
###########3


############## grafica
Iteration <- c(1:(ciclos+1))
Accuracy <- Accuracyk5h5
grafico <- data.frame(Iteration,Accuracy)
ggplot(grafico,aes(x = Iteration,y =Accuracy)) + geom_line(size=1.5,alpha=0.5) +
  scale_x_continuous(breaks =  Iteration ,labels=itera) +
  coord_cartesian(ylim=c(0, 1)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

Accuracy
#########



qwer<- data.frame(neurons)
qwer <- data.frame(qwer,neurons)


qwer[,c(11:20)]


sqrt(44)
