#include <Rcpp.h>
#include "neuron.h"
using namespace Rcpp;

/**
 * Este archivo contiene funciones que son llamadas desde R,
 * ademas contiene implementaciones para CKTSOM que actualmente no estan siendo utilizadas
 */


///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

/**
 * Returns a list of indices indicating the location of the children for a given neuron.
 *
 */
Rcpp::NumericVector getChildrenIndices(const int neuron,const int  numberOfChildrenperNode){
  Rcpp::NumericVector children(numberOfChildrenperNode);
  for (int i = 1; i <=numberOfChildrenperNode ;i++){
    //la expresion (-i+1) proviene de una relacion entre el indice i la ubicaicon del hijo respectivo
    children[numberOfChildrenperNode -i] = numberOfChildrenperNode*(neuron+1)+(-i+1);  //disminuye el numero para una lista que empieza en 0
  }
  return children;
}

/**
 * Returns a indicex of the father of a given neuron.
 * Warning: The C code uses indices starting from 0, but in R the indices IDs start from 1.
 */
int getParentIndex(const int neuron,const int numberOfChildrenperNode){
  int father = (neuron + numberOfChildrenperNode - 1 )/numberOfChildrenperNode; //N - (N%k)
  return father-1;  // we must subtract unity because the root index starts with 0
}


/**
 * Returns a vector of indices identifying all the siblings, including the neuron itself.
 **/
Rcpp::NumericVector getSiblingsIndices(const int neuron ,const int numberOfChildrenperNode){
  int father = getParentIndex(neuron,numberOfChildrenperNode);
  Rcpp::NumericVector brothers = getChildrenIndices(father,numberOfChildrenperNode);
  return brothers;
}


// [[Rcpp::export]]
float calculateEuclideanDistance (const DataFrame point1,const DataFrame point2){
  NumericMatrix point1Matrix = internal::convert_using_rfunction(point1, "as.matrix");
  NumericVector point1Vector = point1Matrix(0,_);

  NumericMatrix point2Matrix = internal::convert_using_rfunction(point2, "as.matrix");
  NumericVector point2Vector = point2Matrix(0,_);
  return calculateEuclideanDistance2PointSquare(point1Vector,point2Vector);
}


// [[Rcpp::export]]
NumericVector findBmuAndDistance(DataFrame dataNeuron,DataFrame dataStimulus,int numberOfChildrenperNode, int treeHeight){
  NumericVector result(2);

  NumericMatrix dataNeuronMatrix = internal::convert_using_rfunction(dataNeuron, "as.matrix");

  NumericMatrix dataStimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
  NumericVector stimulus = dataStimulusMatrix(0,_);
  //find BMU
  result[0] = findBMU_tree( stimulus,dataNeuronMatrix, numberOfChildrenperNode, treeHeight);

  //Distance to BMU
  result[1] = calculateEuclideanDistance2PointSquare(dataNeuronMatrix(result[0],_),stimulus);
  return result;
}

// [[Rcpp::export]]
int hardFindBmu(DataFrame dataNeuron,DataFrame dataStimulus,int numberOfChildrenperNode, int treeHeight){
  NumericVector result(2);

  NumericMatrix dataNeuronMatrix = internal::convert_using_rfunction(dataNeuron, "as.matrix");

  NumericMatrix dataStimulusMatrix = internal::convert_using_rfunction(dataStimulus, "as.matrix");
  NumericVector stimulus = dataStimulusMatrix(0,_);

  int lastfather = dataNeuronMatrix(_,0).size()-pow(numberOfChildrenperNode,treeHeight);
  NumericMatrix neuronsChildren(pow(numberOfChildrenperNode,treeHeight),stimulus.size());
  for(int i = 0; i < neuronsChildren(_,0).size(); i++){
    neuronsChildren(i,_) = dataNeuronMatrix(lastfather+i,_);
  }


    //calcula la distancia de el estimulo a las neuronas del nivel
  NumericVector dist = distance(neuronsChildren,stimulus);
  NumericVector::iterator it = std::min_element(dist.begin(), dist.end());

  //cambia el BMU al hijo mas cercano
  return lastfather + it - dist.begin();

}
