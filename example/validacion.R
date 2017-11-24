#Carga los datos
dataMat <- readMat("http://homepage.tudelft.nl/n9d04/occ/519/oc_519.mat")
################################################          1           ################################
###########   obtiene el RMSE para el dato cargado, entregando un rango y la iteracion
################################################          1           ################################
## set parameters
data <-data.frame(dataMat$x$data)
trainSettings <- getDefaultTraingSettings(numberOfChildrenperNode = 3,treeHeight = 3,
                                          initialLearningRate =1 ,finalLearningRate = 0.0,
                                          initialRadius = 3,finalRadius = 0)
total <- 10   ## cantidad de pruebas
iteraciones <- c(1:total)
RMSE <- c(1:total)
iteracion <- 10    #  iteracion inicial

for ( i in iteraciones){
  setSeed(543)
  trainSettings[7] <-iteracion
  RMSE[i]<- calculateRMSE(data,trainSettings)    # Calcula el RMSE
  iteraciones[i] <- iteracion
  iteracion <- iteracion * 4    ## INtervalos de iteraciones
  print(i)}

RMSE  #muestra en pantalla los valores de RMSE
iteraciones  #muestra en pantalla las iteraciones para los RMSE mostrados


################################################          2           ################################
###########   Grafica los RMSE obtenidos arriba
################################################          2           ################################
Iteration <- c(1:total)
grafico <- data.frame(Iteration,RMSE)
ggplot(grafico,aes(x = Iteration,y =RMSE)) + geom_line(size=1.5,alpha=0.5) +
  scale_x_continuous(breaks =  Iteration ,labels=iteraciones) +
  theme(axis.text.x=element_text(angle=90,hjust=1))


################################################          3           ################################
###########   Calculos de AUC
################################################          3           ################################
labels <- dataMat$x$nlab
strata <- calculateStrata(labels,0.5)
#trainSettings   esta definido en el punto 1
trainSettings[7] <-  10240   ## iteracion obtenida analizando los resultados del RMSE
howManyAuc <- 20 # cuantas veces se hace el experimento
vectorStandartDesviation <- c(0.1,seq(0.5, 3, 0.5))  ## se genera una secuencia de distinos valores para el z-score   (0.1 0.5 1.0 1.5 2.0 2.5 3.0)

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
grafic<- data.frame(vectorStandartDesviation,  meanAuc)
plot <-ggplot(grafic, aes_string(x = "vectorStandartDesviation", y = "meanAuc"))+ geom_line(size = 1.6,alpha= 0.5)


plot + xlab("Sigma") + ylab ("AUC") +
  scale_y_continuous(breaks=seq(0, 1, 0.1),limit = c(0,1))

maxAUC <-max(meanAuc)
maxAUC * 100  ##AUC maximo
matc <- match(maxAUC,meanAuc)
sd(out[matc,])*100  # desviacion estandar
vectorStandartDesviation[matc] # Theta usado en el Z-score
c(trainSettings)
c(maxAUC * 100,sd(out[matc,])*100,vectorStandartDesviation[matc],RMSE[which(iteraciones == trainSettings[7]) ])


