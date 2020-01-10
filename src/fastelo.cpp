// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp ;

//' @export
// [[Rcpp::export]]
List fastelo(CharacterVector WINNER,
             CharacterVector LOSER,
             CharacterVector ALLIDS,
             NumericVector KVALS,
             NumericVector STARTVALUES,
             bool NORMPROB = true,
             bool ROUND = true) {

  // initialize needed objects
  NumericVector winprobs(WINNER.size()) ;

  // results object (initialize with startvalues)
  NumericVector svals = clone<NumericVector>(STARTVALUES) ;

  double loserrat = 0.0 ;
  double winnerrat = 0.0 ;
  double pscore = 0.0 ;
  double kp = 0.0 ;
  double divi = 0.0 ;


  for (int seqindex=0; seqindex < WINNER.size(); seqindex++) {
    // get current ratings
    for (int k = 0; k < ALLIDS.size(); k++) {
      if (ALLIDS[k] == LOSER[seqindex])  { loserrat = svals[k] ; }
      if (ALLIDS[k] == WINNER[seqindex]) { winnerrat = svals[k] ; }
    }

    // winning probabilities following 'normal' or 'exponential' approach
    if(NORMPROB == true) {
      pscore = Rf_pnorm5( (winnerrat - loserrat) / (200 * sqrt(2.0)), 0.0, 1.0, 1, 0) ;
    }
    if(NORMPROB == false) {
      divi = (winnerrat - loserrat)/400.0 ;
      pscore =  1 - 1 / (1 + pow(10.0, divi)) ;
    }

    // calculate points distributed and update contestants' ratings
    // either with or without rounding to integers
    winprobs[seqindex] = pscore ;
    kp = KVALS[seqindex] * pscore;
    if(ROUND == true) {
      loserrat = round(loserrat + kp - KVALS[seqindex]);
      winnerrat = round(winnerrat - kp + KVALS[seqindex]);
    }
    if(ROUND == false) {
      loserrat = loserrat + kp - KVALS[seqindex];
      winnerrat = winnerrat - kp + KVALS[seqindex];
    }

    // update ratings in results object
    for(int k = 0; k < ALLIDS.size(); k++) {
      if(ALLIDS[k] == LOSER[seqindex]) { svals[k] = loserrat; }
      if(ALLIDS[k] == WINNER[seqindex]) { svals[k] = winnerrat; }
    }

  }

  // naming the ratings vector and create another object to allow identifying that the returned list comes from the fastelo function
  svals.names() = ALLIDS;
  CharacterVector rtype = CharacterVector::create("fastelo");
  Rcpp::List result = Rcpp::List::create(Rcpp::Named("ratings") = svals,
                                         Rcpp::Named("winprobs") = winprobs,
                                         Rcpp::Named("rtype") = rtype,
                                         Rcpp::Named("startvalues") = STARTVALUES,
                                         Rcpp::Named("kvalues") = KVALS,
                                         Rcpp::Named("winner") = WINNER,
                                         Rcpp::Named("loser") = LOSER,
                                         Rcpp::Named("allids") = ALLIDS,
                                         Rcpp::Named("normprob") = NORMPROB,
                                         Rcpp::Named("round") = ROUND);
  result.attr("class") = "fastelo";
  return result;
}
