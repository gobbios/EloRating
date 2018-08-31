#' Directional Consistency Index
#'
#' calculate Directional Consistency Index
#'
#' @param interactionmatrix square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return numeric value, the DCI
#'
#' @references
#' \insertRef{vanhooff1987}{EloRating}
#' @author Christof Neumann
#' @export
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' mat <- creatematrix(SEQ)
#' DCindex(mat)
#'
#' # or directly from a matrix
#' data(bonobos)
#' DCindex(bonobos)

DCindex <- function(interactionmatrix) {
  uptri <- interactionmatrix[upper.tri(interactionmatrix)]
  lotri <- t(interactionmatrix)[upper.tri(interactionmatrix)]
  x <- cbind(uptri, lotri)
  x <- t(apply(x, 1, sort, decreasing = TRUE))
  return( (sum(x[, 1]) - sum(x[, 2]) ) / sum(x))
}
