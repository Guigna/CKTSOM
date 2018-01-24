#ifndef PKG_FOO1_H
#define PKG_FOO1_H
#include <Rcpp.h>

int findBMU_tree(Rcpp::NumericVector, Rcpp::NumericMatrix , int , int );
double calculateEuclideanDistance2PointSquare (Rcpp::NumericVector , Rcpp::NumericVector  );
Rcpp::NumericVector distance (Rcpp::NumericMatrix , Rcpp::NumericVector );

#endif
