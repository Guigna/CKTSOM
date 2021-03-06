#include <Rcpp.h>
#include <string>
#include <stdlib.h>     /* srand, rand */
#include <time.h>
#include <math.h>

#include <algorithm>


using namespace Rcpp;



/**
* Compute the number of neurons in the tree considering the depth of the tree as well as the number of children per node.
*/
int calculateNumberOfNeurons(int numberOfChildrenperNode, int treeHeight){
  int sum = 0 ;
  for (int i = 0 ; i <= treeHeight ; i++ ){
    sum = sum + pow(numberOfChildrenperNode,i);
  }
  return sum;
}


/**
* Adjust the whole tree using SOMs update rule.
**/
NumericMatrix updateTree(NumericMatrix neurons, const NumericVector stimulus,
                         long double radius, const float learningRate,const int BMU, const int numberOfChildrenperNode){
  LogicalVector mask = is_na(stimulus);
  int children;
  int current = BMU;
  long double factor = learningRate;
  //update neuron current
  for (int i =0 ; i <neurons(current,_).size(); i++){
    if (mask[i] != true){
      neurons(current,i) = neurons(current,i) - factor* (neurons(current,i) - stimulus[i]);
    }
  }
  factor = factor*0.9;
  while(current > 0 && radius >= 1){

    int currentFatherIndex = ((current + numberOfChildrenperNode - 1 )/numberOfChildrenperNode)-1;
    //neuron update father of current
    for (int i =0 ; i <neurons(currentFatherIndex,_).size(); i++){
      if (mask[i] != true){
        neurons(currentFatherIndex,i) = neurons(currentFatherIndex,i) - factor* (neurons(currentFatherIndex,i) - stimulus[i]);
      }
    }
    for (int i = 1; i <=numberOfChildrenperNode && radius>=1 ;i++){
      //la expresion (-i+1) proviene de una relacion entre el indice i la ubicaicon del hijo respectivo
      children = numberOfChildrenperNode*(currentFatherIndex+1)+(-i+1);  //disminuye el numero para una lista que empieza en 0

      if (children != current){
        //neuron update Siblings of current
        for (int i =0 ; i <neurons(children,_).size(); i++){
          if (mask[i] != true){
            neurons(children,i) = neurons(children,i) - (factor*0.2) * (neurons(children,i) - stimulus[i]);
          }
        }

      }
    }
    radius = radius - 1;
    factor = factor*0.9;
    current = currentFatherIndex;
  }
  return neurons;
}


//calcula la distancia eucludiana al cuadrado entre 2 puntos   (neuron, stimulus)
double calculateEuclideanDistance2PointSquare (const NumericVector point1,const NumericVector point2 ){
  LogicalVector mask = is_na(point2);
  double sum=0;
  for (int i = 0; i < mask.size(); i++){
    if (mask[i] != true){
      sum = sum +  pow(( point1[i] - point2[i]),2);
    }
  }
  return sum;
}


//calcula la distancia del estimulo a las neuronas
NumericVector distance (const NumericMatrix neurons,const NumericVector stimulus){

  NumericVector distances(neurons(_,1).size());
  for (int i = 0; i <distances.size();i++ ){
    distances(i) = calculateEuclideanDistance2PointSquare(neurons(i,_),stimulus);
  }
  return distances;
}

//Busca el BMU partiendo en la raiz, llegando a las hojas
int findBMU_tree(const NumericVector stimulus,const NumericMatrix neurons,const int numberOfChildrenperNode,const int treeHeight){
  int BMU = 0;
  int lastfather = (neurons(_,0).size()-1)-pow(numberOfChildrenperNode,treeHeight);
  int olderChildrenIndice;
  while (BMU <= lastfather){
    //find first children
    olderChildrenIndice = numberOfChildrenperNode*(BMU+1)-numberOfChildrenperNode+1;
    //genera un vector con las neuronas del nivel
    NumericMatrix neuronsChildren(numberOfChildrenperNode,stimulus.size());
    for(int i = 0; i < numberOfChildrenperNode; i++){
      neuronsChildren(i,_) = neurons(olderChildrenIndice+i,_);
    }
    //calcula la distancia de el estimulo a las neuronas del nivel
    NumericVector dist = distance(neuronsChildren,stimulus);
    NumericVector::iterator it = std::min_element(dist.begin(), dist.end());
    //cambia el BMU al hijo mas cercano
    BMU = olderChildrenIndice + it - dist.begin();
  }
  return BMU;
}


//random uniforme
inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// desordena un vector
Rcpp::NumericVector randomShuffle(Rcpp::NumericVector a) {
  std::random_shuffle(a.begin(), a.end(), randWrapper);
  return a;
}


// genera un vector creciente de 0 a size lo desordena al retornar
NumericVector generatevector(int size){
  NumericVector index(size);
  for (int i=0; i<size; i++) { index[i] = i; };
  index = randomShuffle(index);

  return index;
}




// [[Rcpp::export]]
Rcpp::DataFrame train_Rcpp(int numberOfChildrenperNode,int treeHeight,float initialLearningRate ,float finalLearningRate,
                       int initialRadius,int  finalRadius, unsigned long iterations
                         , Rcpp::List lst,
                         Rcpp::CharacterVector Names = Rcpp::CharacterVector::create()) {

  int columnLength = lst.size();
  int neuronsSize = calculateNumberOfNeurons(numberOfChildrenperNode,treeHeight);
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
  int maxData = data(_,0).size()-1;
  int indexDato ;

  LogicalVector mask;

  for (int i = 0; i < neuronsSize; i++) {
    indexDato = minD + ((double) rand() / (RAND_MAX)) * (maxData - minD);

    mask = is_na(data(indexDato,_));
    if(sum(mask) != 0){  //NA in DATA
      i = i-1;
    } else{
      neurons(i,_) = data(indexDato,_);
    }
  }
  //neurons listas para mover

  long double learningRate = initialLearningRate;
  long double learningRateStep = (initialLearningRate - finalLearningRate) / iterations;
  long double radius = initialRadius;
  long double radiusStep = (initialRadius - finalRadius) / iterations;


  //inicializa la epoca
  int index = 0;
  int dataLength = data(_,0).size();

  NumericVector vectorIndex = generatevector(dataLength);

  ///////////////////////////////
  ///////////////////////////////START TRAINING
  ///////////////////////////////

  for(unsigned long i = 0 ; i <iterations ; i++){
    //inicia nueva epoca
    if (index == dataLength){
      vectorIndex = randomShuffle(vectorIndex);
      index = 0;
    }

    //busca el BMU
    int bestNeuron = findBMU_tree( data(vectorIndex(index),_), neurons,numberOfChildrenperNode, treeHeight);
    //actualiza la red neuronal
    neurons = updateTree( neurons,  data(vectorIndex(index),_),round(radius),  learningRate, bestNeuron, numberOfChildrenperNode);
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

// [[Rcpp::export]]
void set_seed(int seed){
  srand(seed);
}





