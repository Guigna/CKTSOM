#' train entrena la red neuronal usando una topologia de arbol
#'
#' @param numberOfChildrenperNode A integer number
#' @param treeHeight A integer number
#' @param initialLearningRate A float number
#' @param finalLearningRate A float number
#' @param initialRadius A integer number
#' @param finalRadius A integer number
#' @param numberOfIterations A integer number
#' @param data A data frame
#'
#'
#' @return La red neuronal entrenada con topologia de un arbol k ario completo
#' @examples
#' library(ggplot2)
#' ##################### EXAMPLE 1 : IRIS DATASET
#' ###parameters
#' numberOfIterations <- 600000
#' initialLearningRate <- 1
#' finalLearningRate<- 0
#' initialRadius <- 7
#' finalRadius <- 1
#' numberOfChildrenperNode <- 2
#' treeHeight <- 3
#'
#' ##training phase
#' data(iris)
#' data<-iris[-5] ## load a dataset
#' ti <- proc.time() # start watch
#' neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)
#' tf <-proc.time()    # stop watch
#' tf-ti #print execution time
#'
#' ##visualization phase
#' graficar(data,neurons,numberOfChildrenperNode) #plot the scatter plot
train <- function(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data){
  neurons <- train_Rcpp(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations,as.list(data), names(data))
  return(neurons)
}


trainSOM <- function(numberColumn, numberRow, initialLearningRate, finalLearningRate,initialRadius, finalRadius,numberOfIterations, data){
  neurons <- trainSOM_Rcpp(numberColumn, numberRow, initialLearningRate, finalLearningRate,initialRadius, finalRadius,numberOfIterations,as.list(data), names(data))
  return(neurons)
}




## el BMU para cada dato del data set

#' calculateBMUForData genera una lista con el BMU para cada dato ingresado
#'
#' @param data A data frame
#' @param neurons A data frame
#' @param clusterVector A numeric vector
#' @param numberOfChildrenperNode A integer number
#' @param treeHeight A integer number
#'
#'
#' @return numeric vector con el BMU de cada dato
calculateBMUForData <- function(data,neurons,clusterVector,numberOfChildrenperNode,treeHeight) {
  dataBMU<- rep(0,length(data[,1]))
  ini<-(length(neurons[,1])-(numberOfChildrenperNode**treeHeight)+1)
  fin<-length(neurons[,1])
  for (i in c(1:length(dataBMU))) {
    dataBMU[i]<-findBMU(neurons[c(ini:fin),],data[i,])
  }
  dataBMU<- dataBMU+ini-1
  for(i in 1:length(dataBMU)){
    dataBMU[i]<- clusterVector[dataBMU[i]]
  }
  return(dataBMU)
}

#' calculateDistance calcula la distancia entre 2 puntos
#'
#' @param point1 A data frame
#' @param point2 A data frame
#'
#' @return float que contiene la distancia calculada
calculateDistance <- function(point1, point2){
  return (calculateEuclideanDistance (point1[1,],point2[1,] ))
}

#' setSeed calcula define la semilla
#'
#' @param semilla A integer
setSeed <- function(semilla=123){
  set_seed(semilla) ## c++
  set.seed(semilla) ## R
}

#' calculateBMUandDistance obtiene el BMU y la distancia a este
#'
#' @param dataNeuron A data frame
#' @param dataStimulus A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#'
#' @return Matrix donde Matrix[1] son los BMU y Matrix[2] contiene las distancias
calculateBMUandDistance <- function(dataNeuron,dataStimulus,numberOfChildrenperNode,treeHeight){
  result <- findBmuAndDistance(dataNeuron,dataStimulus,numberOfChildrenperNode,treeHeight)
  result[1] <- result[1] + 1  ##c++ usa las listas empezando en 0, se aumenta en 1 para poder usarlas sin problema en R
  return(result)
}

#' calculateGroups agrupa los datos definido por los grupos que se pueden generar con un arbol determinado
#'
#' @param numberOfGroups A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer

#'
#' @return NumericVector que contiene el grupo para cada neurona del arbol
calculateGroups <- function(numberOfGroups,numberOfChildrenperNode,treeHeight){
  level <- 0
  levelGroup <- numberOfChildrenperNode**level
  while (levelGroup < numberOfGroups){
    level<- level +1
    levelGroup <- numberOfChildrenperNode**level
  }
  size <- calculateNumberOfNeurons(numberOfChildrenperNode,treeHeight)
  id <- 1
  groups <- rep(id,size)

  ini <- min(buscaHijos(level,numberOfChildrenperNode))
  fin <- ini + numberOfChildrenperNode ** (level) -1
  for(i in c(ini:fin)){
    id <- id+1
    groups[i] <- id
    groups <- marcarHijos(i,numberOfChildrenperNode,treeHeight,size,groups )
  }
  return(groups)
}

#' marcarHijos registra el grupo del nodo
#'
#' @param node A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#' @param size A integer
#' @param groups A integer
#'
#' @return NumericVector que contiene el grupo para cada neurona del arbol
marcarHijos<- function(node,numberOfChildrenperNode,treeHeight,size,groups ){
  hijos <- buscaHijos(node,numberOfChildrenperNode)
  if(hijos[1]<size){
    for(i in hijos){
      groups[i] <- groups[node]
      groups <- marcarHijos(i,numberOfChildrenperNode,treeHeight,size,groups )
    }
  }
  return(groups)
}

#' calculateNumberOfNeurons calcula el largo del arbol
#'
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#'
#' @return integer largo del arbol
calculateNumberOfNeurons<- function( numberOfChildrenperNode, treeHeight){
  sum <- 0
  for (i in c(0:treeHeight)) {
    sum <- sum + numberOfChildrenperNode**i;
  }
  return(sum)
}

#' getOutliers calcula los outla
#'
#' @param neurons A data frame
#' @param data A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#' @param howManyStandardDeviations A integer [1:3]
#'
#' @return
getOutliers <- function(neurons,data ,numberOfChildrenperNode,treeHeight,howManyStandardDeviations = 1){
  clusterVector<- c(1:length(neurons[,1]))
  ## calculate the bmu and euclidian distance for each data
  result <-matrix(ncol = 2,nrow = length(data[,1]))  #create matrix to start BMU and euclidean distance
  for (i in 1:length(data[,1])) {
    stimulus <- data[i,]
    result[i,]<-calculateBMUandDistance(neurons,stimulus, numberOfChildrenperNode, treeHeight)
  }
  ## calculate mu and sigma
  mu <- mean(result[,2])
  sigma <- sd(result[,2])

  #generate  Z-Score
  #|(d - (mu) )|  / (sigma)
  Zscore <- vector(length = length(result[,1]))  #create vector withc lenght equals to the number of stimuli
  Zscore<- (abs(result[,2] - mu))/sigma
  ##get outliers
  #z-score < [12,3]
  outliers <- c(1:length(data[,1]))
  outliers <- outliers[Zscore > howManyStandardDeviations]
  return(outliers)
}

calculateBmuDistance <- function(neurons,data ,numberOfChildrenperNode,treeHeight){
  clusterVector<- c(1:length(neurons[,1]))
  ## calculate the bmu and euclidian distance for each data
  result <-matrix(ncol = 2,nrow = length(data[,1]))  #create matrix to start BMU and euclidean distance
  for (i in 1:length(data[,1])) {
    stimulus <- data[i,]
    result[i,]<-calculateBMUandDistance(neurons,stimulus, numberOfChildrenperNode, treeHeight)
  }

  return(result)
}

getOutliersMuSigma <- function(result,mu,sigma,howManyStandardDeviations = 1){

  #generate  Z-Score
  #|(d - (mu) )|  / (sigma)
  Zscore <- vector(length = length(result[,1]))  #create vector withc lenght equals to the number of stimuli
  Zscore<- (abs(result[,2] - mu))/sigma
  ##get outliers
  #z-score < [12,3]
  outliers <- c(1:length(result[,1]))
  outliers <- outliers[Zscore > howManyStandardDeviations]
  return(outliers)
}

calculateMatrixOfConfusion <- function(neurons,data,numberOfChildrenperNode,treeHeight,mu,sigma,dataTraining,vectorStandartDEsviation){
  totalCalculado<-data.frame()
  result <- calculateBmuDistance(neurons,data ,numberOfChildrenperNode,treeHeight)
  for (howManyStandardDeviations in vectorStandartDEsviation) {


    outliers<-getOutliersMuSigma(result,mu,sigma,howManyStandardDeviations)
    procesData <- data[-outliers,]
    largoDAta <- length(data[,1])
    nn <- length(outliers)

    todo <- rbind(procesData,dataTraining)
    duplicado<-duplicated(todo)
    duplicado[duplicado == TRUE] <- 1
    vp <-sum(duplicado)
    fn<-length(procesData[,1])-vp

    ##los negativos o otliers
    procesDataNegative <- data[outliers,]
    todoNegativ<-  rbind(procesDataNegative,dataTraining)
    duplicado<-duplicated(todoNegativ)
    duplicado[duplicado == TRUE] <- 1
    fp<-sum(duplicado)
    vn<-length(procesDataNegative[,1])-fp

    calculo<- data.frame(vp,fn,fp,vn,howManyStandardDeviations)
    totalCalculado<- rbind(totalCalculado,calculo)

  }
  return(totalCalculado)
}

getDefaultTraingSettings<-function(numberOfChildrenperNode = 3,treeHeight =3,
                                 initialLearningRate=1,finalLearningRate=0,initialRadius=7,
                                 finalRadius=1,numberOfIterations=600000){
  trainSeting<- rep(0,7)
  trainSeting[1] <- numberOfChildrenperNode
  trainSeting[2] <- treeHeight
  trainSeting[3] <- initialLearningRate
  trainSeting[4] <- finalLearningRate
  trainSeting[5] <- initialRadius
  trainSeting[6] <- finalRadius
  trainSeting[7] <- numberOfIterations

  return(trainSeting)
}

validate<-function(data,labels,strataConfig,standardDeviations,trainSettings,howManyAuc = 5){
  repMax <- 10*howManyAuc
  repAct<-1
  aucFinal <- rep(NA,howManyAuc)
  numberOfChildrenperNode <- trainSettings[1]
  treeHeight <- trainSettings[2]
  initialLearningRate <- trainSettings[3]
  finalLearningRate <- trainSettings[4]
  initialRadius <- trainSettings[5]
  finalRadius <- trainSettings[6]
  numberOfIterations <- trainSettings[7]

  labels <- labels -1
  columns<- c(1:(length(data)+1))
  i <- 1



  while (i<=howManyAuc) {

    ##train
    #dataTraining<- data.frame(data,labels)


    #estratos <- strata( dataTraining, stratanames = c("labels"), size = strataConfig, method = "srswor" )
    #dataTraining <- getdata( dataTraining, estratos )
    dataTraining <- data[labels ==1,]

    idTrain<-sample( 1:nrow( dataTraining ), strataConfig )

    dataTraining <- data[idTrain,]
    #dataTraining <- dataTraining[dataTraining$labels ==1,]


    #trainingFolk <- dataTraining[,columns]
    #dataTraining <- dataTraining[,-length(dataTraining)]


    #######train listo               training

    # Training with dataTraining
    neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, dataTraining)
    # Calculus of mu and sigma
    result <- calculateBmuDistance(neurons,dataTraining ,numberOfChildrenperNode,treeHeight)
    mu <- mean(result[,2])
    sigma <- sd(result[,2])

    #########data test

    #originalDataTestLabels<- data.frame(data,labels)
    dataTest <- data[-idTrain,]
    resultadoEsoperado <- labels[-idTrain]
    dataTest <- dataTest[,-length(dataTest)]

    result <- calculateBmuDistance(neurons,dataTest ,numberOfChildrenperNode,treeHeight)
    outliers<-getOutliersMuSigma(result,mu,sigma,howManyStandardDeviations = standardDeviations)



    resultadoObtenido <- rep(1,length(dataTest[,1]))
    resultadoObtenido[outliers] <- 0

    testLabels = resultadoEsoperado
    predictedScores = resultadoObtenido
    if (sum(predictedScores) == 0 | sum(predictedScores) == length(predictedScores)) {
      i <- i-1
    } else {
      pred <- prediction(testLabels, predictedScores);
      pred <- performance(pred,"auc");
      auc  <- pred@y.values[[1]][1]
      aucFinal[i] <-auc
    }
    i <- i+1
    repAct<- repAct+1
    if (repAct > repMax){
      i <- 999
    }
  }
  return(aucFinal)
}




calculateStrata<- function(label,percentage){
  label <- label -1
  return (round( sum(label) * percentage))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalizeDataFRame<- function(data) {
  dim <- length(data[1,])
  for (i in c(1:dim)){
    data[,i] <- normalize( data[,i])
  }
  data[data==NaN] <- 0
  return(data)
}

oneRMSE<- function(data,trainSettings){

  numberOfChildrenperNode <- trainSettings[1]
  treeHeight <- trainSettings[2]
  initialLearningRate <- trainSettings[3]
  finalLearningRate <- trainSettings[4]
  initialRadius <- trainSettings[5]
  finalRadius <- trainSettings[6]
  numberOfIterations <- trainSettings[7]

  neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data)

  out <- matrix(, nrow = length(data[,1]), ncol = 2)
  for(i in c(1:length(data[,1]))){
    out[i,] <- findBmuAndDistance(neurons,data[i,],numberOfChildrenperNode,treeHeight)
  }
  RMSE <- sqrt(mean(out[,2]))  ### el out[2] esta elevado

  return (RMSE)
}

pruebas<- function(dataMat,iniLearn,hijosPerNode,iteraciones){
  totalPruebas<- 9

  data <-data.frame(dataMat$x$data)
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
    #maxAUC * 100  ##AUC maximo
    matc <- match(maxAUC,meanAuc)
    #sd(out[matc,])*100  # desviacion estandar
    #vectorStandartDesviation[matc] # Theta usado en el Z-score
    #c(trainSettings)
    #c(maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc])

    resultados[prueba,]<-c(trainSettings,maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc])

  }
  return(resultados)
}

#####################
calculateRMSE <- function(data,trainSettings,log = c(1:3),seed = 543){
  iteraciones <- c(1:length(log))
  RMSEout <- c(1:length(log))
  for ( i in iteraciones){
    setSeed(seed)
    trainSettings[7] <-log[i]
    RMSEout[i]<- oneRMSE(data,trainSettings)    # Calcula el RMSE
    iteraciones[i] <- log[i]
    #print(i)
  }

  return(data.frame(RMSEout,iteraciones))
  #RMSE  #muestra en pantalla los valores de RMSE
  #iteraciones  #muestra en pantalla las iteraciones para los RMSE mostrados
}
############

calculateAUC<- function(data,labels,
                        numberOfChildrenperNodeList=c(2),
                        treeHeightList = c(3,4),
                        initialLearningRateList = c(1,0.8,0.5),
                        finalLearningRateList = c(0.0),
                        initialRadiusList = c(3,6),
                        finalRadiusList = c(0),
                        numberOfIterationsList= c(1000,10000),
                        seed = 543){

  totalPruebas <- length(numberOfChildrenperNodeList) *
    length(treeHeightList)*
    length(initialLearningRateList)*
    length(finalLearningRateList)*
    length(initialRadiusList)*
    length(finalRadiusList)*
    length(numberOfIterationsList)
  print("Total pruebas:")
  print(totalPruebas)


  strata <- calculateStrata(labels,0.5)

  howManyAuc <- 20 # cuantas veces se hace el experimento
  vectorStandartDesviation <- c(0.1,seq(0.5, 3, 0.5))  ## se genera una secuencia de distinos valores para el z-score   (0.1 0.5 1.0 1.5 2.0 2.5 3.0)

  resultados <-matrix(ncol = 10,nrow = totalPruebas)
  #iterar
  nn<-1
  ##########
  for (numberOfchildren in numberOfChildrenperNodeList) {
    for (treeHig in treeHeightList) {
      for (iniLearn in initialLearningRateList) {
        for (finalLearn in finalLearningRateList) {
          for (iniRadius in initialRadiusList) {
            for (finaRadiu in finalRadiusList) {
              for (numbreOfItera in numberOfIterationsList) {
                trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = numberOfchildren,treeHeight = treeHig,
                                                          initialLearningRate =iniLearn ,finalLearningRate = finalLearn,
                                                          initialRadius = iniRadius,finalRadius = finaRadiu,
                                                          numberOfIterations = numbreOfItera)
                out <- matrix(NA, nrow=length(vectorStandartDesviation), ncol=howManyAuc)
                n<- 1
                for (standardDeviations in vectorStandartDesviation) {
                  setSeed(seed)
                  aucCalculate <- validate(data,labels,strata,standardDeviations,trainSettings,howManyAuc) ## una ejecucion

                  out[n,] <- aucCalculate
                  n<- n+1
                }
                #print(prueba)
                ## Mean AUC
                meanAuc <- c(1:length(out[,1]))
                for (i in c(1:length(out[,1]))) {
                  meanAuc[i]<- mean(out[i,])
                }


                maxAUC <-max(meanAuc)
                matc <- match(maxAUC,meanAuc)
                resultados[nn,]<-c(trainSettings,maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc])

                print(nn)
                nn <- nn+1
              }

            }

          }

        }

      }
    }

  }
  print("k h ai af ri rf T AUC sigma theta")
  return(resultados)

}


