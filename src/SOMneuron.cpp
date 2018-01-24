#include <Rcpp.h>
#include "neuron.h"

using namespace Rcpp;

/**
 * Implementacion basica de SOM
 */

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

//metodos para topologia de grafo
//busca el BMU entre todas las neuronas
// [[Rcpp::export]]
int findBMU(DataFrame dataNeuron,DataFrame dataStimulus){
  Rcpp::NumericMatrix neurons = internal::convert_using_rfunction(dataNeuron, "as.matrix");
  NumericMatrix stimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
  NumericVector stimulusVector = stimulusMatrix(0,_);
  //Datos listos para trabajar en matriz
  NumericVector range = distance(neurons, stimulusVector);
  NumericVector::iterator it = std::min_element(range.begin(), range.end());

  return it - range.begin() + 1;
}


/**
 * The SOM's update rule.
 * neuron: a vector containing the weights of the neuron to be updated.
 * stimulus: a random selected instance obtained form the dataset.
 * learningRate: a factor specifying the ammount of the migration towards the stimulus.
 **/
Rcpp::NumericVector updateNeuron(Rcpp::NumericVector neuron, const Rcpp::NumericVector &stimulus,const long double learningRate){
  for (int i =0 ; i <neuron.size(); i++){
    neuron[i] = neuron[i] - learningRate* (neuron[i] - stimulus[i]);
  }
  return neuron;
}




//actualiza el matriz de SOM
NumericMatrix updateStructureSOM(NumericMatrix neurons, NumericVector stimulus,
                                 float radius, float learningRate, int BMU, int numberColumn,int numberRow, NumericVector vecinoVicitado ){


  //mueve el BMU
  if(vecinoVicitado[BMU] == 0){
    neurons(BMU,_) = updateNeuron(neurons(BMU,_),stimulus,learningRate);
    vecinoVicitado[BMU] = 1;
  }

  radius = radius - 1;
  //disminuye la tasa de aprendizaje, ya que en cada nivel que sube el entrenamiento
  //se encuentra mas lejos del BMU y mas cerca de la raiz
  learningRate = learningRate * 0.9;

  //mueve vecinos
  if (radius >= 1) {
    if(((BMU+1)  % numberColumn != 0) && (BMU+1) < (numberColumn * numberRow)){
      neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU+1,numberColumn,numberRow,vecinoVicitado);
    }

    if(((BMU+1) -1)  % numberColumn != 0 &&  BMU > 1){
      neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU-1,numberColumn,numberRow,vecinoVicitado);
    }

    if((BMU) - numberColumn >= 0){
      neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU - numberColumn ,numberColumn,numberRow,vecinoVicitado);
    }

    if((BMU) + numberColumn < (numberColumn * numberRow)){
      neurons = updateStructureSOM(neurons,stimulus,radius,learningRate,BMU  + numberColumn ,numberColumn,numberRow,vecinoVicitado);
    }


  }
  return neurons;
}

int findBmuSom(NumericVector stimulusVector,NumericMatrix neurons){

  NumericVector range = distance(neurons, stimulusVector);
  NumericVector::iterator it = std::min_element(range.begin(), range.end());

  return it - range.begin();
}


//desordena el set de datos
NumericMatrix disorder(NumericMatrix data){
  int random1,random2;
  for(int i = 0; i < data(_,0).size()*2/3;i++){
    random1 = rand() % data(_,0).size();
    NumericVector dataTemp = data(random1,_);
    random2 = rand() % data(_,0).size();
    data(random1,_)=data(random2,_);
    data(random2,_)=dataTemp;
  }
  return data;
}


// [[Rcpp::export]]
Rcpp::DataFrame trainSOM_Rcpp(int numberColumn,int numberRow,float initialLearningRate ,float finalLearningRate,
                              int initialRadius,int  finalRadius, int iterations
                                , Rcpp::List lst,
                                Rcpp::CharacterVector Names = Rcpp::CharacterVector::create()) {

  int columnLength = lst.size();
  int neuronsSize = numberColumn * numberRow;
  SEXP ll = lst[0];
  Rcpp::NumericVector y(ll);
  int dataSize = y.size();

  Rcpp::NumericMatrix data(dataSize,columnLength);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    data( _ ,i) =y;
  }
  //Datos listos para trabajar en matriz

  Rcpp::NumericMatrix neurons(neuronsSize,columnLength);

  //genera los datos copiando del dataset
  int minD =0;
  int maxD = neurons(_,0).size()-1;
  int indexDato ;
  for (int i = 0; i < neuronsSize; i++) {
    indexDato = minD + ((double) rand() / (RAND_MAX)) * (maxD - minD);
    neurons(i,_) = data(indexDato,_);

  }
  //neurons listas para mover
  float learningRate = initialLearningRate;

  float learningRateStep = (initialLearningRate - finalLearningRate) / iterations;

  float radius = initialRadius;

  float radiusStep = (initialRadius - finalRadius) / iterations;


  //desordena los datos
  data = disorder(data);
  //inicializa la epoca
  int index = 0;
  int dataLength = data(_,0).size();

  ///////////////////////////////
  ///////////////////////////////START TRAINING
  ///////////////////////////////

  for(int i = 0 ; i <iterations ; i++){
    //inicia nueva epoca
    if (index == dataLength){
      data = disorder(data);
      index = 0;
    }

    //busca el BMU
    int bestNeuron = findBmuSom( data(index,_), neurons);
    //actualiza la red neuronal
    NumericVector vecinoVicitado(neuronsSize);
    neurons = updateStructureSOM( neurons,  data(index,_),round(radius),  learningRate, bestNeuron, numberColumn, numberRow,  vecinoVicitado);

    radius -= radiusStep;
    learningRate -= learningRateStep;
    index+=1;
  }


  ///////////////////////////////
  ///////////////////////////////END TRAINING
  ///////////////////////////////


  //genera el dataFrame para retornar
  Rcpp::List tmp(columnLength);
  Rcpp::CharacterVector lnames = Names.size() < lst.size() ?
  lst.attr("names") : Names;
  Rcpp::CharacterVector names(columnLength);
  Rcpp::List listaTempo(neuronsSize);
  for (int i = 0; i < columnLength; i++) {
    SEXP ll = lst[i];
    Rcpp::NumericVector y(ll);
    dataSize = y.size();
    Rcpp::NumericVector xx = neurons(_,i);
    listaTempo = Rcpp::List::create(Rcpp::Named("vec") = xx);
    tmp[i ] = listaTempo;
    if (std::string(lnames[i]).compare("") != 0) {
      names[i] = lnames[i];
    } else {

      std::string Result;          // string which will contain the result

      std::ostringstream convert;   // stream used for the conversion

      convert << i;      // insert the textual representation of 'Number' in the characters in the stream

      Result = convert.str();
      names[i] = "V" + Result;
    }
  }
  Rcpp::DataFrame result(tmp);
  result.attr("names") = names;

  return result;
}


