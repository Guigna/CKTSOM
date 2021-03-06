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
  ini<-(length(neurons[,1])-(numberOfChildrenperNode**treeHeight)+1)  ## primera neurona BMU
  fin<-length(neurons[,1])                    ## ultima neurona BMU
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
  return (calculateEuclideanDistance (point1[1,],point2[1,] ))#calcula distancia usando c++
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

#' marcarHijos registra el grupo del nodo, utilizado para gaficar
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


#' calculateBmuDistance calcula el BMU y la distancia a esta de instancia del conjunto de datos
#'
#' @param neurons A data frame
#' @param data A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#'
#' @return

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


#' getOutliers calcula los outiers
#'
#' @param neurons A data frame
#' @param data A data frame
#' @param numberOfChildrenperNode A integer
#' @param treeHeight A integer
#' @param howManyStandardDeviations A integer [1:3]
#'
#' @return
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

validate<-function(data,labels,validation_size,standardDeviationsList,trainSettings,howManyAuc = 5){
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

    ## separar datos en entrenamiento y test
    data.target <- data[labels ==1,]

    idTarget<-sample( 1:nrow( data.target ), validation_size )## obteniendo al azar validatio_size elementos objetivo
    data.target <- data.target[idTarget,] # obteniendo el subconjunto de dato de entrenamineto, compuesto exclusivamente de datos objetivo
    idTrain <- as.numeric(row.names(data.target))  # id de datos de entreneamiento con respecto a dataset original


    #######           training

    # Training with dataTraining
    neurons <- train(numberOfChildrenperNode,treeHeight,initialLearningRate,finalLearningRate,initialRadius,finalRadius,numberOfIterations, data.target)
    # Calculus of mu and sigma
    result <- calculateBmuDistance(neurons,data.target ,numberOfChildrenperNode,treeHeight)
    mu <- mean(result[,2])
    sigma <- sd(result[,2])

    #########data test

    #originalDataTestLabels<- data.frame(data,labels)
    dataTest <- data[-idTrain,]
    resultadoEsoperado <- labels[-idTrain]


    result <- calculateBmuDistance(neurons,dataTest ,numberOfChildrenperNode,treeHeight)
    outliers<-getOutliersMuSigma(result,mu,sigma,howManyStandardDeviations = standardDeviationsList)



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


validationSize <- function(data,percentage){
  return(round(dim(data)[1]*percentage))
}

#' normalize Normaliza una lista entre  0 y 1
#'
#' @param x A list
#'
#' @return lista normalizada
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#' normalizeDataFRame Normaliza los datos entre  0 y 1
#'
#' @param data A data frame
#'
#' @return data frame normalizado
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



#####################
calculateRMSE <- function(data,trainSettings,numberOfIterationsList= c(1000,10000),seed = 543){
  n <- 1
  RMSEout <- c(1:length(numberOfIterationsList))
  for ( numberOfIterations in numberOfIterationsList){
    setSeed(seed)
    trainSettings[7] <-numberOfIterations
    RMSEout[n]<- oneRMSE(data,trainSettings)    # Calcula el RMSE
    n <- n+1
    #print(i)
  }

  return(data.frame(RMSEout,numberOfIterationsList))
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
                        vectorStandartDesviationList = c(0.1,seq(0.5, 3, 0.5)),
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



  validation_size <- validationSize(data[labels == 2,],0.5) # entrega la cantidad de datos utilizados para el entrenamiento [0,1]
  howManyAuc <- 20 # cuantas veces se hace el experimento
  vectorStandartDesviation <- vectorStandartDesviationList  ## se genera una secuencia de distinos valores para el z-score   (0.1 0.5 1.0 1.5 2.0 2.5 3.0)
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
                  aucCalculate <- validate(data,labels,validation_size,standardDeviations,trainSettings,howManyAuc) ## una ejecucion

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

####models







cktsom <- setRefClass("cktsom",
                      fields = list(neurons = "data.frame",mean= "numeric",sigma = "numeric",
                                    numberOfChildrenperNode = "numeric",treeHeight = "numeric"),
                      methods = list(
                        prediction = function(data,threshold){
                          result <- calculateBmuDistance(neurons,data ,numberOfChildrenperNode,treeHeight)
                          outliers<-getOutliersMuSigma(result,mean,sigma,howManyStandardDeviations = threshold)
                          resultadoObtenido <- rep(1,length(data[,1]))
                          resultadoObtenido[outliers] <- 0
                          resultadoObtenido<- resultadoObtenido+1
                          return(resultadoObtenido)
                        },
                        getBMUs = function(){
                          size <- length(neurons[,1])
                          first <- size + 1 - numberOfChildrenperNode ** treeHeight
                          return(neurons[c(first:size),])
                        }

                      )
)

buildCKTSOM <- function(data, trainSettings){
  neurons <- train(trainSettings[1],trainSettings[2],trainSettings[3],trainSettings[4],trainSettings[5],trainSettings[6],trainSettings[7], data)

  result <- calculateBmuDistance(neurons,data ,trainSettings[1],trainSettings[2])
  mu <- mean(result[,2])
  sigma <- sd(result[,2])

  c <- cktsom(neurons = neurons, mean = mu, sigma = sigma,numberOfChildrenperNode = trainSettings[1],
              treeHeight = trainSettings[2])
  return(c)
}




