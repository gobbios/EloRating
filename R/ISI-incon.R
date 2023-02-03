# isi 15_07_05

#' number of inconsistencies
#'
#' calculate number of inconsistencies
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return integer, the number of inconsistencies in the matrix
#'
#' @author Christof Neumann
#'
#' @references 
#' \insertRef{devries1998}{EloRating}
#'
#' @examples
#' data(bonobos)
#' EloRating:::.incon(bonobos)
#'

.incon <- function(mat) {
  sum( (mat - t(mat))[upper.tri(mat)] < 0, na.rm = TRUE)
}
