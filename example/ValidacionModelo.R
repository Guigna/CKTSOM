library(CKTSOM)
library(ggplot2)
library(R.matlab)
require(sampling)
#install.packages("R.matlab")
library(dplyr)
require("ROCR")
library(beepr)


trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 3,treeHeight = 2,
                                          initialLearningRate =1 ,finalLearningRate = 0.0,
                                          initialRadius = 3,finalRadius = 0, numberOfIterations =10000 )


setSeed(121)
dataMat <- readMat("http://homepage.tudelft.nl/n9d04/occ/501/oc_501.mat")
data <-data.frame(dataMat$x$data)

data <- data[,c(1,2)]
labels <- dataMat$x$nlab





data.target <- data[labels ==2,] #filtra los que tienen id 2 , porque son los objetivos
validation_size <- validationSize(data.target,0.5)# entrega la cantidad de datos utilizados para el entrenamiento [0,1]




id.target<-sample( 1:nrow( data.target ), validation_size ) ## obteniendo al azar validatio_size elementos objetivo
data.train <- data.target[id.target,] # obteniendo el subconjunto de dato de entrenamineto, compuesto exclusivamente de datos objetivo

id.train <- as.numeric(row.names(data.train)) # id de datos de entreneamiento con respecto a dataset original



#Generar modelo
model <- buildCKTSOM(data[id.train,],trainSettings)



#dataTest <- data[-id.train,]
label.expected <- labels[-id.train]

###Prediccion
model$prediction(data[-id.train,],threshold =3)


###PGrafica
clusterVisualizationZone(data[-id.train,],model,threshold = 10,x = 1,y = 2)
############  + coord_fixed()

