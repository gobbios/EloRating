#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

// [[Rcpp::export]]
NumericVector steepint(NumericMatrix mat, IntegerVector nrand, bool Dij = true) {
  // stuff outside the loop

  int nids = mat.nrow() ; // number of individuals
  int nij ; // needed for loops when calculating number of interactions per dyad

  // clone matrix - make input matrix an 'arma' matrix
  arma::mat omat = as<arma::mat>(Rcpp::clone(mat)) ; // omat: original

  // prepare results
  const int r = nrand[0] ;
  NumericVector steeps(r + 1, 0.0) ; // for 'old' steepness

  // first calculate winning probabilities for original matrix
  arma::mat tmat = trans(omat) ; // tmat: transposed
  arma::mat pmat = omat / omat ; // pmat: proportions

  // Pij
  if(Dij == false) {
    pmat = omat / (tmat + omat) ;
  }

  // calculate Dij (dyadic index corrected for chance, de Vries et al 2006)
  if(Dij == true) {
    for(int i = 0; i < nids; i++) {
      for(int j = 0; j < nids; j++) {
        if(i != j) {
          nij = omat(i, j) + omat(j, i) ;
          pmat(i, j) = omat(i, j) / nij - ((omat(i, j) / nij - 0.5) / (nij + 1) ) ;
        }
      }
    }
  }

  // replace NAs with 0s
  for(int i = 0; i < nids; i++) {
    for(int j = 0; j < nids; j++) {
      if(std::isnan(pmat(j, i))) pmat(j, i) = 0;
    }
  }

  // get rowSums and colSums
  arma::colvec w = sum(pmat, 1); arma::rowvec l = sum(pmat, 0);
  arma::colvec w2 = pmat * w; arma::colvec l2 = trans(pmat) * trans(l);
  // David's scores
  arma::rowvec DS = w.t() + w2.t() - l -l2.t();

  arma::uvec indices = sort_index(arma::conv_to<arma::vec>::from(DS), "descend");
  arma::mat outmat = omat(indices, indices);
  // outmat is the reordered input matrix (sorted according to DS(pij))

  // prep for steepness-regression by hand ... because I can't get 'solve' to work...
  arma::vec A1 = arma::ones<arma::vec>(nids);
  arma::vec A2 = arma::ones<arma::vec>(nids);
  arma::vec B1 = arma::ones<arma::vec>(nids);
  arma::vec AB1 = arma::ones<arma::vec>(nids);
  arma::vec ranks = arma::ones<arma::vec>(nids);
  for(int i=0; i<nids; i++){ ranks(i) = i+1; }
  double addv = nids * (nids - 1)/2.0 ;


  arma::rowvec nDS = (DS + addv) / nids;
  arma::vec nds = arma::sort( arma::conv_to<arma::vec>::from(nDS) , "ascend");
  ranks = arma::sort( ranks , "descend") ;

  for(int i=0; i<nids; i++) { A1(i) = ranks(i) - mean(ranks); }
  for(int i=0; i< nids; i++) { B1(i) = nds(i) - mean(nds); }
  for(int i=0; i<nids; i++) { AB1(i) = A1(i) * B1(i); }
  for(int i=0; i<nids; i++) { A2(i) = A1(i) * A1(i); }
  steeps(0) = arma::accu(AB1) / arma::accu(A2) * (-1.0);



  // run the loop
  arma::mat smat = omat + tmat ;
  arma::mat zmat = arma::zeros<arma::mat>(mat.nrow(),mat.nrow());

  if(r > 0) {
    for(int d=0; d<r; d++){
      // randomize matrix
      smat = outmat + trans(outmat) ;
      zmat = outmat - outmat;
      int mm = mat.nrow();
      for(int i=0; i<mm; i++){
        for(int j=(i+1); j<mm; j++){
          Rcpp::IntegerVector frame = Rcpp::Range(0, smat(i,j));
          arma::uvec KK = as<arma::uvec>(frame) ;
          KK = arma::shuffle(KK) ;
          zmat(i,j) = KK(0);
          zmat(j,i) = smat(j,i) - zmat(i, j) ;
        }
      }

      tmat = trans(zmat);


      // Pij
      if(Dij == false) {
        pmat = zmat / (tmat + zmat) ;
      }

      // calculate Dij (dyadic index corrected for chance, de Vries et al 2006)
      if(Dij == true) {
        for(int i = 0; i < nids; i++) {
          for(int j = 0; j < nids; j++) {
            if(i != j) {
              nij = zmat(i, j) + zmat(j, i) ;
              pmat(i, j) = zmat(i, j) / nij - ((zmat(i, j) / nij - 0.5) / (nij + 1) ) ;
            }
          }
        }
      }

      // replace NAs with 0s
      for(int i = 0; i < nids; i++) {
        for(int j = 0; j < nids; j++) {
          if(std::isnan(pmat(j, i))) pmat(j, i) = 0 ;
        }
      }

      // get rowSums and colSums
      w = sum(pmat, 1); l = sum(pmat, 0); w2 = pmat * w; l2 = trans(pmat) * trans(l);
      DS = w.t() + w2.t() - l - l2.t();
      indices = sort_index(arma::conv_to<arma::vec>::from(DS), "descend");
      // reorder
      outmat = zmat(indices, indices);

      nDS = (DS + addv) / nids;

      nds = arma::sort( arma::conv_to<arma::vec>::from(nDS) , "ascend");
      ranks = arma::sort( ranks , "descend") ;
      for(int i=0; i<nids; i++){ A1(i) = ranks(i) - mean(ranks); }
      for(int i=0; i<nids; i++){ B1(i) = nds(i) - mean(nds); }
      for(int i=0; i<nids; i++){ AB1(i) = A1(i) * B1(i); }
      for(int i=0; i<nids; i++){ A2(i) = A1(i) * A1(i); }
      steeps(d+1) = arma::accu(AB1) / arma::accu(A2) * (-1.0);

    }
  }

  return steeps;
}
