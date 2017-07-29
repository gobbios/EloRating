#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

// [[Rcpp::export]]
List mat2seqint(NumericMatrix mat) {
  int N = sum(mat) ; // total number of interactions in matrix
  int n = mat.nrow() ; // total number of individuals in matrix
  int cellval = 0 ; // temporary value to store the value of a given cell during the loop
  int counter = 0 ; // counter to track location in output vectors

  IntegerVector winners(N, 0) ; // results vectors
  IntegerVector losers(N, 0) ;

  for(int i = 0; i < n; i++) {
    for(int j = 0; j < n; j++) {
      cellval = mat(i, j);
      if(cellval > 0) {
        for(int cv = 0; cv < cellval; cv++) {
          winners(counter) = i + 1 ; // adding 1 so that indexing works directly in R
          losers(counter) = j + 1 ;
          counter = counter + 1;
        }
      }
    }
  }

  return Rcpp::List::create(winners, losers);
}
