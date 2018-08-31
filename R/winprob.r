#' expected probability of winning
#'
#' calculate expected probability of winning given known strength of both opponents
#'
#' @param elo1 Elo rating from individual for which the winning probability should be calculated
#' @param elo2 Elo rating of the opponent
#' @param normprob logical (by default \code{TRUE}). Should a normal curve be assumed for calculating the winning/losing probablities, or a logistic curve.
#'
#' @details Elo (1978) proposed three ways of calculating winning probabilities (section 8.73), one of which (the `linear' approach) is ignored here because it ``lacks the sophistication and flexibility to express the limitation on D [rating difference] and the deflation controls required for integrity of the ratings''. Between the two remaining approaches (normal and logistic), Elo favored initially the normal over the logistic function, though he writes that the logistic function ``better reflects large deviations in an extended series''. Because of Elo's initial preference, the default approach taken by the package's functions is the normal one, though it can be changed to the logistic one if desired.
#'
#' @return numeric, expected chance of first individual to win an interacation with the second individual
#'
#' @references
#' \insertRef{elo1978}{EloRating}
#'
#' @author Christof Neumann
#'
#' @importFrom stats pnorm
#'
#' @examples
#' winprob(1200,1000)
#' winprob(1000,1200)
#' winprob(1000,1000)
#'
#' @export


winprob <- function(elo1, elo2, normprob=TRUE) {

  if (normprob) {
    # z score based on fixed SD=200 (see Elo 1978)
    z_score <- (elo1 - elo2) / (200 * sqrt(2))
    p_win <- pnorm(z_score)
  } else {
    p_win <- 1 - 1 / ( 1 + 10 ^ ( (elo1 - elo2) / 400 ) )
  }

  return(p_win)
}
