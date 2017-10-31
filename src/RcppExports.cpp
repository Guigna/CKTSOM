// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// train_Rcpp
Rcpp::DataFrame train_Rcpp(int numberOfChildrenperNode, int treeHeight, float initialLearningRate, float finalLearningRate, int initialRadius, int finalRadius, unsigned long iterations, Rcpp::List lst, Rcpp::CharacterVector Names);
RcppExport SEXP CKTSOM_train_Rcpp(SEXP numberOfChildrenperNodeSEXP, SEXP treeHeightSEXP, SEXP initialLearningRateSEXP, SEXP finalLearningRateSEXP, SEXP initialRadiusSEXP, SEXP finalRadiusSEXP, SEXP iterationsSEXP, SEXP lstSEXP, SEXP NamesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type numberOfChildrenperNode(numberOfChildrenperNodeSEXP);
    Rcpp::traits::input_parameter< int >::type treeHeight(treeHeightSEXP);
    Rcpp::traits::input_parameter< float >::type initialLearningRate(initialLearningRateSEXP);
    Rcpp::traits::input_parameter< float >::type finalLearningRate(finalLearningRateSEXP);
    Rcpp::traits::input_parameter< int >::type initialRadius(initialRadiusSEXP);
    Rcpp::traits::input_parameter< int >::type finalRadius(finalRadiusSEXP);
    Rcpp::traits::input_parameter< unsigned long >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type Names(NamesSEXP);
    rcpp_result_gen = Rcpp::wrap(train_Rcpp(numberOfChildrenperNode, treeHeight, initialLearningRate, finalLearningRate, initialRadius, finalRadius, iterations, lst, Names));
    return rcpp_result_gen;
END_RCPP
}
// set_seed
void set_seed(int seed);
RcppExport SEXP CKTSOM_set_seed(SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    set_seed(seed);
    return R_NilValue;
END_RCPP
}
// calculateEuclideanDistance
float calculateEuclideanDistance(const DataFrame point1, const DataFrame point2);
RcppExport SEXP CKTSOM_calculateEuclideanDistance(SEXP point1SEXP, SEXP point2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DataFrame >::type point1(point1SEXP);
    Rcpp::traits::input_parameter< const DataFrame >::type point2(point2SEXP);
    rcpp_result_gen = Rcpp::wrap(calculateEuclideanDistance(point1, point2));
    return rcpp_result_gen;
END_RCPP
}
// findBmuAndDistance
NumericVector findBmuAndDistance(DataFrame dataNeuron, DataFrame dataStimulus, int numberOfChildrenperNode, int treeHeight);
RcppExport SEXP CKTSOM_findBmuAndDistance(SEXP dataNeuronSEXP, SEXP dataStimulusSEXP, SEXP numberOfChildrenperNodeSEXP, SEXP treeHeightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dataNeuron(dataNeuronSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type dataStimulus(dataStimulusSEXP);
    Rcpp::traits::input_parameter< int >::type numberOfChildrenperNode(numberOfChildrenperNodeSEXP);
    Rcpp::traits::input_parameter< int >::type treeHeight(treeHeightSEXP);
    rcpp_result_gen = Rcpp::wrap(findBmuAndDistance(dataNeuron, dataStimulus, numberOfChildrenperNode, treeHeight));
    return rcpp_result_gen;
END_RCPP
}
// findBMU
int findBMU(DataFrame dataNeuron, DataFrame dataStimulus);
RcppExport SEXP CKTSOM_findBMU(SEXP dataNeuronSEXP, SEXP dataStimulusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dataNeuron(dataNeuronSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type dataStimulus(dataStimulusSEXP);
    rcpp_result_gen = Rcpp::wrap(findBMU(dataNeuron, dataStimulus));
    return rcpp_result_gen;
END_RCPP
}
// trainSOM_Rcpp
Rcpp::DataFrame trainSOM_Rcpp(int numberColumn, int numberRow, float initialLearningRate, float finalLearningRate, int initialRadius, int finalRadius, int iterations, Rcpp::List lst, Rcpp::CharacterVector Names);
RcppExport SEXP CKTSOM_trainSOM_Rcpp(SEXP numberColumnSEXP, SEXP numberRowSEXP, SEXP initialLearningRateSEXP, SEXP finalLearningRateSEXP, SEXP initialRadiusSEXP, SEXP finalRadiusSEXP, SEXP iterationsSEXP, SEXP lstSEXP, SEXP NamesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type numberColumn(numberColumnSEXP);
    Rcpp::traits::input_parameter< int >::type numberRow(numberRowSEXP);
    Rcpp::traits::input_parameter< float >::type initialLearningRate(initialLearningRateSEXP);
    Rcpp::traits::input_parameter< float >::type finalLearningRate(finalLearningRateSEXP);
    Rcpp::traits::input_parameter< int >::type initialRadius(initialRadiusSEXP);
    Rcpp::traits::input_parameter< int >::type finalRadius(finalRadiusSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type Names(NamesSEXP);
    rcpp_result_gen = Rcpp::wrap(trainSOM_Rcpp(numberColumn, numberRow, initialLearningRate, finalLearningRate, initialRadius, finalRadius, iterations, lst, Names));
    return rcpp_result_gen;
END_RCPP
}
